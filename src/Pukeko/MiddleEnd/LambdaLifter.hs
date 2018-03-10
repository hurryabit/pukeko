module Pukeko.MiddleEnd.LambdaLifter
  ( liftModule
  )
where

import Pukeko.Prelude

import           Control.Monad.Freer.Supply
import           Data.List         (sortOn)
import qualified Data.Map.Extended as Map
import qualified Data.Set          as Set

import           Pukeko.AST.SystemF
import           Pukeko.AST.SuperCore hiding (Module (..), Expr, Bind, Altn, Par)
import qualified Pukeko.AST.SuperCore as Core
import           Pukeko.AST.Language
import           Pukeko.AST.Name
import           Pukeko.AST.ConDecl
import           Pukeko.FrontEnd.Gamma
import           Pukeko.FrontEnd.Info
import           Pukeko.AST.Type

type In  = Unclassy
type Out = SuperCore

type CanLL effs =
  ( CanGamma effs
  , Members [ Supply Int
            , Reader ModuleInfo
            , Reader TmVar
            , Writer Core.Module, NameSource
            ]
    effs
  )
type LL a = forall effs. CanLL effs => Eff effs a

freshTmVar :: LL TmVar
freshTmVar = do
  func <- ask @TmVar
  n <- fresh @Int
  mkName (Lctd noPos (fmap (\x -> x ++ "$ll" ++ show n) (nameText func)))

coercePar :: Par In -> Par Out
coercePar = \case
  TmPar x -> TmPar x
  TyPar v -> TyPar v

-- NOTE: It might be interesting to prove some stuff about indices here using
-- Liquid Haskell.
-- | Lift a value abstraction to the top level.
--
-- Let's say we're in some environment Γ and have an expression
--
-- > F = λ(x₁:τ₁) … (xₙ:τₙ). E
--
-- Let y₁, …, yₘ be the value variables captured by F, i.e., the free value
-- variables of E which are not among the xᵢ. Suppose Γ contains the typings
-- (y₁:σ₁), …, (yₘ:σₘ). Let a₁, …, aₖ be the type variables captured by F, i.e.,
-- the type variables which are free in E or one of the σᵢ or one of the τⱼ.
-- Finally, assume the type of E is υ, i.e.,
--
-- > Γ, (x₁:τ₁), …, (xₙ:τₙ) ⊢ E : υ
--
-- and
--
-- > Γ ⊢ F : τ₁ → ⋯ τₙ → υ
--
-- In this situation, we create a new top level declaration
--
-- > f : ∀a₁ … aₖ. σ₁ → ⋯ → σₘ → τ₁ → ⋯ τₙ → υ =
-- >   Λa₁ … aₖ. λ(y₁:σ₁) … (yₘ:σₘ) (x₁:τ₁) … (xₙ:τₙ). E
--
-- and replace F with the partial application
--
-- > F' = f @a₁ … @aₖ y₁ … yₘ
--
-- Last but not least, note that if we sort the aᵢ and yⱼ by thei de Bruijn
-- indices in decreasing order, we create more opportunities for η-reduction,
-- which is good for inlining.
llETmAbs :: [Par In] -> Type -> Expr Out -> LL (Expr Out)
llETmAbs oldPars t_rhs rhs1 = do
    let evCaptured0 = Set.toList
          (setOf freeTmVar rhs1 `Set.difference`
           setOf (traverse . _TmPar . to nameOf) oldPars)
    (evCaptured, newTmPars)  <-
      fmap (unzip . map snd . sortOn fst) . for evCaptured0 $ \x -> do
        (t, i) <- lookupTmVarIx x
        bind <- (, t) <$> copyName noPos x
        pure (i, (x, bind))
    let allPars0 = map TmPar newTmPars ++ map coercePar oldPars
    -- NOTE: We're (ab)using the fact that a subterm @λ(x:t). Λa. E@ of a closed
    -- term without shadowing of type variables has the same free type variables
    -- as @Λa. λ(x:t). E@. In other words, moving all the type abstractions in
    -- front of the ter abstractions does not change the set of free type
    -- variables.
    let tvCaptured = Set.toList
          ((setOf (traverse . _TmPar . _2 . traverse) allPars0 <> setOf traverse t_rhs)
           `Set.difference` setOf (traverse . _TyPar) oldPars)
    -- TODO: Sort type variable binders in descreasing de Bruijn index order.
    tyPars <- traverse (copyName noPos) tvCaptured
    let tvMap = Map.fromList (zipExact tvCaptured tyPars)
    let tvRename :: TyVar -> TyVar
        tvRename v = Map.findWithDefault v v tvMap
    let evMap = map nameOf newTmPars
                & zipExact evCaptured
                & Map.fromList
    let evRename :: TmVar -> TmVar
        evRename x = Map.findWithDefault x x evMap
    let allPars1 =
          map TyPar tyPars ++ over (traverse . _TmPar . _2 . traverse) tvRename allPars0
    let rhs2 = over expr2type (fmap tvRename) (over freeTmVar evRename rhs1)
    lhs <- freshTmVar
    let t_lhs = rewindr mkTAbs allPars1 (fmap tvRename t_rhs)
    -- TODO: We could use the pos of the lambda for @lhs@.
    let supc = SupCDecl lhs (closeT t_lhs) allPars1 rhs2
    tell (mkFuncDecl @Any supc)
    pure (foldl ETmApp (foldl ETyApp (EVal lhs) (map TVar tvCaptured)) (map EVar evCaptured))

llExpr :: Expr In -> LL (Expr Out)
llExpr = \case
  ELoc le -> llExpr (unlctd le)
  EVar x -> pure (EVar x)
  EAtm a -> pure (EAtm a)
  EApp fun (TmArg arg) -> ETmApp <$> llExpr fun <*> llExpr arg
  EMat t  cs -> EMat <$> llExpr t <*> traverse llAltn cs
  ELet BindPar ds e0 ->
    ELet BindPar
    <$> (traverse . b2bound) llExpr ds
    <*> introTmVars (map _b2binder ds) (llExpr e0)
  ELet BindRec ds e0 ->
    introTmVars (map _b2binder ds) $
      ELet BindRec <$> (traverse . b2bound) llExpr ds <*> llExpr e0
  elam@EAbs{}
    | (pars, ETyAnn t_rhs rhs0) <- unwindEAbs elam -> do
        rhs1 <- introPars pars (llExpr rhs0)
        llETmAbs pars t_rhs rhs1
  EAbs{} -> impossible  -- the type inferencer puts types around lambda bodies
  ECast coe e0 -> ECast coe <$> llExpr e0
  EApp e0 (TyArg t) -> ETyApp <$> llExpr e0 <*> pure t
  ETyAnn _ e0 -> llExpr e0

llAltn :: Altn In -> LL (Altn Out)
llAltn (MkAltn (PSimple dcon ts0 bs) e) = do
  (MkTyConDecl _ vs _, MkTmConDecl _ _ _ flds0) <- findInfo info2tmcons dcon
  assertM (length vs == length ts0)  -- the kind checker guarantees this
  let env0 = Map.fromList (zipExact vs ts0)
  let t_flds = fmap (>>= (env0 Map.!)) flds0
  let env = catMaybes (zipWithExact (\b t -> (,) <$> b <*> pure t) bs t_flds)
  MkAltn (PSimple dcon ts0 bs) <$> introTmVars env (llExpr e)

llDecl :: Members [Reader ModuleInfo, Writer Core.Module, NameSource] effs =>
  Decl In -> Eff effs ()
llDecl = \case
  DType t -> tell (mkTypeDecl t)
  DFunc (MkFuncDecl lhs t rhs) -> do
    rhs <- llExpr rhs & runGamma & evalSupply @Int [1 ..] & runReader lhs
    let supc = SupCDecl lhs (closeT t) [] rhs
    tell (mkFuncDecl @Any supc)
  DExtn (MkExtnDecl z t s) -> tell (mkFuncDecl @Any (ExtnDecl z (closeT t) s))

liftModule :: Member NameSource effs => Module In -> Eff effs Core.Module
liftModule m0@(MkModule decls) =
  traverse_ llDecl decls
  & runInfo m0
  & runWriter
  & fmap snd
