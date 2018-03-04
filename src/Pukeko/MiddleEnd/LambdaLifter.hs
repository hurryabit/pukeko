module Pukeko.MiddleEnd.LambdaLifter
  ( liftModule
  )
where

import Pukeko.Prelude

import           Control.Monad.Freer.Supply
import           Data.List         (sortOn)
import qualified Data.List.NE      as NE
import qualified Data.Map.Extended as Map
import qualified Data.Set          as Set

import           Pukeko.AST.SystemF
import           Pukeko.AST.SuperCore hiding (Module (..), Expr, Bind, Altn)
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
            , Reader NameEVar
            , Writer Core.Module, NameSource
            ]
    effs
  )
type LL a = forall effs. CanLL effs => Eff effs a

freshEVar :: LL (Name EVar)
freshEVar = do
  func <- ask @NameEVar
  n <- fresh @Int
  mkName (Lctd noPos (fmap (\x -> x ++ "$ll" ++ show n) (nameText func)))

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
llETmAbs :: NonEmpty (EVarBinder Type) -> Type -> Expr Out -> LL (Expr Out)
llETmAbs oldBinds t_rhs rhs1 = do
    let evCaptured0 = Set.toList
          (setOf freeEVar rhs1 `Set.difference` setOf (traverse . to nameOf) oldBinds)
    (evCaptured, newBinds)  <-
      fmap (unzip . map snd . sortOn fst) . for evCaptured0 $ \x -> do
        (t, i) <- lookupEVarIx x
        bind <- (, t) <$> copyName noPos x
        pure (i, (x, bind))
    let allBinds0 = newBinds ++ toList oldBinds
    let tvCaptured = Set.toList
          (setOf (traverse . _2 . traverse) allBinds0 <> setOf traverse t_rhs)
    -- TODO: Sort type variable binders in descreasing de Bruijn index order.
    tyBinds <- traverse (copyName noPos) tvCaptured
    let tvMap = Map.fromList (zipExact tvCaptured tyBinds)
    let tvRename :: NameTVar -> NameTVar
        tvRename v = Map.findWithDefault v v tvMap
    let evMap = map nameOf newBinds
                & zipExact evCaptured
                & Map.fromList
    let evRename :: NameEVar -> NameEVar
        evRename x = Map.findWithDefault x x evMap
    let allBinds1 = over (traverse . _2 . traverse) tvRename allBinds0
    let rhs2 = over expr2type (fmap tvRename) (over freeEVar evRename rhs1)
    lhs <- freshEVar
    let t_lhs = rewindr TUni' tyBinds (map snd allBinds1 *~> fmap tvRename t_rhs)
    -- TODO: We could use the pos of the lambda for @lhs@.
    let supc = SupCDecl lhs (closeT t_lhs) tyBinds allBinds1 rhs2
    tell (mkFuncDecl @Any supc)
    pure (foldl ETmApp (foldl ETyApp (EVal lhs) (map TVar tvCaptured)) (map EVar evCaptured))

llExpr :: Expr In -> LL (Expr Out)
llExpr = \case
  ELoc le -> llExpr (unlctd le)
  EVar x -> pure (EVar x)
  EAtm a -> pure (EAtm a)
  EApp fun (TmArg arg) -> ETmApp <$> llExpr fun <*> llExpr arg
  EMat t  cs -> EMat <$> llExpr t <*> traverse llAltn cs
  ELet ds e0 ->
    ELet
    <$> (traverse . b2bound) llExpr ds
    <*> withinEScope (map _b2binder ds) (llExpr e0)
  ERec ds e0 ->
    withinEScope (map _b2binder ds) $
      ERec <$> (traverse . b2bound) llExpr ds <*> llExpr e0
  elam@(EAbs TmPar{} _)
    | (oldBinds, ETyAnn t_rhs rhs0) <- unwindETmAbs elam -> do
        rhs1 <- withinEScope oldBinds (llExpr rhs0)
        llETmAbs (NE.fromList oldBinds) t_rhs rhs1
  EAbs TmPar{} _ -> impossible  -- the type inferencer puts types around lambda bodies
  ECast coe e0 -> ECast coe <$> llExpr e0
  EApp e0 (TyArg t) -> ETyApp <$> llExpr e0 <*> pure t
  EAbs (TyPar v) e0 -> ETyAbs v <$> withinTScope1 v (llExpr e0)
  ETyAnn _ e0 -> llExpr e0

llAltn :: Altn In -> LL (Altn Out)
llAltn (MkAltn (PSimple dcon ts0 bs) e) = do
  (MkTConDecl _ vs _, MkDConDecl _ _ _ flds0) <- findInfo info2dcons dcon
  assertM (length vs == length ts0)  -- the kind checker guarantees this
  let env0 = Map.fromList (zipExact vs ts0)
  let t_flds = fmap (>>= (env0 Map.!)) flds0
  let env = catMaybes (zipWithExact (\b t -> (,) <$> b <*> pure t) bs t_flds)
  MkAltn (PSimple dcon ts0 bs) <$> withinEScope env (llExpr e)

llDecl :: Members [Reader ModuleInfo, Writer Core.Module, NameSource] effs =>
  Decl In -> Eff effs ()
llDecl = \case
  DType t -> tell (mkTypeDecl t)
  DFunc (MkFuncDecl lhs t rhs) -> do
    rhs <- llExpr rhs & runGamma & evalSupply @Int [1 ..] & runReader lhs
    let supc = SupCDecl lhs (closeT t) [] [] rhs
    tell (mkFuncDecl @Any supc)
  DExtn (MkExtnDecl z t s) -> tell (mkFuncDecl @Any (ExtnDecl z (closeT t) s))

liftModule :: Member NameSource effs => Module In -> Eff effs Core.Module
liftModule m0@(MkModule decls) =
  traverse_ llDecl decls
  & runInfo m0
  & runWriter
  & fmap snd
