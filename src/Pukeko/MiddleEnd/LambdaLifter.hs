module Pukeko.MiddleEnd.LambdaLifter
  ( liftModule
  )
where

import Pukeko.Prelude

import           Control.Monad.Freer.Supply
import qualified Data.List.NE      as NE
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import qualified Safe.Exact        as Safe

import           Pukeko.AST.SystemF
import           Pukeko.AST.SuperCore hiding (Module (..), Expr, Defn, Altn, Bind)
import qualified Pukeko.AST.SuperCore as Core
import           Pukeko.AST.Language
import           Pukeko.AST.ConDecl
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.FrontEnd.Gamma
import           Pukeko.FrontEnd.Info
import           Pukeko.AST.Type

type In  = Unclassy
type Out = SuperCore

type IsTVar tv = (Ord tv, BaseTVar tv)
type IsEVar ev = (Ord ev, BaseEVar ev, HasEnv ev)

type LL tv ev =
  EffGamma tv ev
    [Reader ModuleInfo, Reader SourcePos, Supply Id.EVar, Writer Core.Module]

execLL :: Module In -> LL Void Void () -> Core.Module
execLL m0 =
  snd . run . runWriter . evalSupply [] . runReader noPos . runInfo m0 . runGamma

-- NOTE: It might be interesting to proof some stuff about indices here using
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
-- Last but not least, not that if we sort the aᵢ and yⱼ by /decreasing/ de
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
    -- TODO: We should figure out a location for @x@.
    newBinds <- for evCaptured $ \x -> MkBind (Lctd noPos (baseEVar x)) <$> lookupEVar x
    let allBinds0 = newBinds ++ toList oldBinds
    let tvCaptured = Set.toList
          (setOf (traverse . bind2type . traverse) allBinds0 <> setOf traverse t_rhs)
    tyBinds <- traverse (\v -> MkQVar <$> lookupQual v <*> pure (baseTVar v)) tvCaptured
    let tvMap = ifoldMap (\j v -> Map.singleton v (mkBound j (baseTVar v))) tvCaptured
    let tvRename :: tv -> TScope Int Void
        tvRename v = Map.findWithDefault (bugWith "tvRename" (baseTVar v)) v tvMap
    let evMap = ifoldMap (\i x -> Map.singleton x (mkBound i (baseEVar x))) evCaptured
    let evRename :: EScope Int ev -> EScope Int Void
        evRename = scope'
                   (\x -> Map.findWithDefault (bugWith "evRename" (baseEVar x)) x evMap)
                   (mkBound . (n0+))
    let allBinds1 = map (fmap tvRename) allBinds0
    let rhs2 = bimap tvRename evRename rhs1
    lhs <- fresh
    let t_lhs = mkTUni tyBinds (map _bind2type allBinds1 *~> fmap tvRename t_rhs)
    -- TODO: We could use the pos of the lambda for @lhs@.
    let supc = SupCDecl (MkBind (Lctd noPos lhs) t_lhs) tyBinds allBinds1 rhs2
    tell (mkFuncDecl @'SupC supc)
    pure (foldl EApp (mkETyApp (EVal lhs) (map TVar tvCaptured)) (map EVar evCaptured))

llExpr ::
  forall tv ev. (HasEnv tv, IsTVar tv, IsEVar ev) =>
  Expr In tv ev -> LL tv ev (Expr Out tv ev)
llExpr = \case
  ELoc le -> here le $ llExpr (unlctd le)
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
  ELam{} -> bug "lambda without type annotation around body during lambda lifting"
  ETyCoe c e0 -> ETyCoe c <$> llExpr e0
  ETyApp e0 ts -> ETyApp <$> llExpr e0 <*> pure ts
  ETyAbs qvs e0 -> ETyAbs qvs <$> withQVars qvs (llExpr e0)
  ETyAnn c e0 -> ETyAnn c <$> llExpr e0

llAltn ::
  (HasEnv tv, IsTVar tv, IsEVar ev) => Altn In tv ev -> LL tv ev (Altn Out tv ev)
llAltn (MkAltn (PSimple dcon ts0 bs) e) = do
  (MkTConDecl _ vs _, MkDConDecl _ _ _ flds0) <- findInfo info2dcons dcon
  unless (length vs == length ts0)
    (bug "wrong number of type arguments for type constructor")
  let t_flds = fmap (instantiateN' ts0) flds0
  let env = Map.fromList
            (catMaybes (Safe.zipWithExact (\b t -> (,) <$> b <*> pure t) bs t_flds))
  MkAltn (PSimple dcon ts0 bs) <$> withinEScope id env (llExpr e)

llDecl :: Decl In -> LL Void Void ()
llDecl = \case
  DType ds -> tell (foldMap mkTypeDecl ds)
  DDefn (MkDefn (MkBind lhs t) rhs) -> do
    resetWith (Id.freshEVars "ll" (lhs^.lctd))
    rhs <- llExpr rhs
    let supc = SupCDecl (MkBind lhs (fmap absurd t)) [] [] (bimap absurd absurd rhs)
    tell (mkFuncDecl @'SupC supc)
  DExtn (MkExtnDecl z s) -> tell (mkFuncDecl @'Extn (ExtnDecl z s))

liftModule :: Module In -> Core.Module
liftModule m0@(MkModule decls) = execLL m0 (traverse_ llDecl decls)
