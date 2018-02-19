module Pukeko.MiddleEnd.LambdaLifter
  ( liftModule
  )
where

import Pukeko.Prelude

import           Control.Monad.Freer.Supply
import qualified Data.List.NE      as NE
import qualified Data.Map.Extended as Map
import qualified Data.Set          as Set

import           Pukeko.AST.SystemF
import           Pukeko.AST.SuperCore hiding (Module (..), Expr, Defn, Altn, Bind)
import qualified Pukeko.AST.SuperCore as Core
import           Pukeko.AST.Language
import           Pukeko.AST.Name
import           Pukeko.AST.ConDecl
import           Pukeko.FrontEnd.Gamma
import           Pukeko.FrontEnd.Info
import           Pukeko.AST.Type

type In  = Unclassy
type Out = SuperCore

type IsTVar tv = (Ord tv, BaseTVar tv)
type IsEVar ev = (Ord ev, BaseEVar ev, HasEnv ev)

type LL tv ev =
  EffGamma tv ev
    [Supply Int, Reader ModuleInfo, State (Name EVar), Writer Core.Module, NameSource]

freshEVar :: LL tv ev (Name EVar)
freshEVar = do
  func <- get @(Name EVar)
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
-- Last but not least, note that if we sort the aᵢ and yⱼ by /decreasing/ de
-- Bruijn indices, we create more opportunities for η-reduction, which is good
-- for inlining.
llELam ::
  forall tv ev. (HasEnv tv, IsTVar tv, IsEVar ev) =>
  NonEmpty (Bind Type tv) ->
  Type tv ->
  Expr Out tv (EScope Int ev) ->
  LL tv ev (Expr Out tv ev)
llELam oldBinds t_rhs rhs1 = do
    let evCaptured = Set.toList (setOf (traverse . _Free) rhs1)
    let n0 = length evCaptured
    newBinds <- for evCaptured $ \x ->
      MkBind <$> copyName noPos (baseEVar x) <*> lookupEVar x
    let allBinds0 = newBinds ++ toList oldBinds
    let tvCaptured = Set.toList
          (setOf (traverse . bind2type . traverse) allBinds0 <> setOf traverse t_rhs)
    tyBinds <- for tvCaptured $ \v ->
      MkQVar <$> lookupQual v <*> copyName noPos (baseTVar v)
    let tvMap = map _qvar2tvar tyBinds
                & zipWithFrom mkBound 0
                & zipExact tvCaptured
                & Map.fromList
    let tvRename :: tv -> TScope Int Void
        tvRename v = tvMap Map.! v
    let evMap = map nameOf newBinds
                & zipWithFrom mkBound 0
                & zipExact evCaptured
                & Map.fromList
    let evRename :: EScope Int ev -> EScope Int Void
        evRename = scope' (evMap Map.!) (mkBound . (n0+))
    let allBinds1 = map (fmap tvRename) allBinds0
    let rhs2 = bimap tvRename evRename rhs1
    lhs <- freshEVar
    let t_lhs = mkTUni tyBinds (map _bind2type allBinds1 *~> fmap tvRename t_rhs)
    -- TODO: We could use the pos of the lambda for @lhs@.
    let supc = SupCDecl lhs t_lhs tyBinds allBinds1 rhs2
    tell (mkFuncDecl @Any supc)
    pure (foldl EApp (mkETyApp (EVal lhs) (map TVar tvCaptured)) (map EVar evCaptured))

llExpr ::
  forall tv ev. (HasEnv tv, IsTVar tv, IsEVar ev) =>
  Expr In tv ev -> LL tv ev (Expr Out tv ev)
llExpr = \case
  ELoc le -> llExpr (unlctd le)
  EVar x -> pure (EVar x)
  EAtm a -> pure (EAtm a)
  EApp fun arg -> EApp <$> llExpr fun <*> llExpr arg
  EMat t  cs -> EMat <$> llExpr t <*> traverse llAltn cs
  ELet ds e0 ->
    ELet
    <$> (traverse . defn2expr) llExpr ds
    <*> withinEScope' (_bind2type . _defn2bind) ds (llExpr e0)
  ERec ds e0 ->
    withinEScope' (_bind2type . _defn2bind) ds $
      ERec <$> (traverse . defn2expr) llExpr ds <*> llExpr e0
  elam@ELam{}
    | (oldBinds, ETyAnn t_rhs rhs0) <- unwindELam elam -> do
        rhs1 <- withinEScope' _bind2type oldBinds (llExpr rhs0)
        llELam (NE.fromList oldBinds) t_rhs rhs1
  ELam{} -> impossible  -- the type inferencer puts type anns around lambda bodies
  ETyCoe c e0 -> ETyCoe c <$> llExpr e0
  ETyApp e0 ts -> ETyApp <$> llExpr e0 <*> pure ts
  ETyAbs qvs e0 -> ETyAbs qvs <$> withQVars qvs (llExpr e0)
  ETyAnn c e0 -> ETyAnn c <$> llExpr e0

llAltn ::
  (HasEnv tv, IsTVar tv, IsEVar ev) => Altn In tv ev -> LL tv ev (Altn Out tv ev)
llAltn (MkAltn (PSimple dcon ts0 bs) e) = do
  (MkTConDecl _ vs _, MkDConDecl _ _ _ flds0) <- findInfo info2dcons dcon
  assertM (length vs == length ts0)  -- the kind checker guarantees this
  let t_flds = fmap (instantiateN' ts0) flds0
  let env = Map.fromList
            (catMaybes (zipWithExact (\b t -> (,) <$> b <*> pure t) bs t_flds))
  MkAltn (PSimple dcon ts0 bs) <$> withinEScope id env (llExpr e)

llDecl :: Decl In -> LL Void Void ()
llDecl = \case
  DType t -> tell (mkTypeDecl t)
  DFunc (MkFuncDecl lhs t rhs) -> do
    put @(Name EVar) lhs
    rhs <- llExpr rhs
    let supc = SupCDecl lhs (fmap absurd t) [] [] (bimap absurd absurd rhs)
    tell (mkFuncDecl @Any supc)
  DExtn (MkExtnDecl z t s) -> tell (mkFuncDecl @Any (ExtnDecl z t s))

liftModule :: Member NameSource effs => Module In -> Eff effs Core.Module
liftModule m0@(MkModule decls) =
  for_ decls (\decl -> llDecl decl & runGamma & evalSupply [1 ..])
  & runInfo m0
  -- FIXME: Get rid of this 'undefined'.
  & runState undefined
  & runWriter
  & delayNameSource
  & fmap snd
