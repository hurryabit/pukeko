module Pukeko.MiddleEnd.LambdaLifter
  ( Out
  , liftModule
  )
where

import Pukeko.Prelude

import           Control.Monad.Freer.Supply
import qualified Data.Map          as Map
import qualified Data.Set          as Set

import           Pukeko.AST.SystemF
import qualified Pukeko.AST.Stage      as St
import           Pukeko.AST.ConDecl
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.FrontEnd.Gamma
import           Pukeko.FrontEnd.Info
import           Pukeko.AST.Type

type In  = St.ClassEliminator
type Out = St.LambdaLifter

type IsTVar tv = (Ord tv, BaseTVar tv)
type IsEVar ev = (Ord ev, BaseEVar ev, HasEnv ev)

type LL tv ev =
  EffGamma tv ev
    [Reader ModuleInfo, Reader SourcePos, Supply Id.EVar, Writer [Decl Out]]

execLL :: Module In -> LL Void Void () -> [Decl Out]
execLL m0 =
  snd . run . runWriter . evalSupply [] . runReader noPos . runInfo m0 . runGamma

yield :: Decl Out -> LL tv ev ()
yield decl = tell [decl]

llExpr ::
  forall tv ev. (HasEnv tv, IsTVar tv, IsEVar ev) =>
  Expr In tv ev -> LL tv ev (Expr Out tv ev)
llExpr = \case
  ELoc le -> here le $ ELoc <$> lctd llExpr le
  EVar x -> pure (EVar x)
  EVal z -> pure (EVal z)
  ECon c -> pure (ECon c)
  ENum n -> pure (ENum n)
  EApp t  us -> EApp <$> llExpr t <*> traverse llExpr us
  ECas t  cs -> ECas <$> llExpr t <*> traverse llCase cs
  ELet ds e0 ->
    ELet
    <$> (traverse . defn2exprSt) llExpr ds
    <*> withinEScope' (_bind2type . _defn2bind) ds (llExpr e0)
  ERec ds e0 ->
    withinEScope' (_bind2type . _defn2bind) ds $
      ERec <$> (traverse . defn2exprSt) llExpr ds <*> llExpr e0
  -- TODO: It might be nice to use Liquid Haskell here.
  ELam oldBinds rhs0 t_rhs -> do
    rhs1 <- withinEScope' _bind2type oldBinds (llExpr rhs0)
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
    let t_lhs = map _bind2type allBinds1 *~> fmap tvRename t_rhs
    -- TODO: We could use the pos of the lambda for @lhs@.
    yield (DSupC (MkSupCDecl (Lctd noPos lhs) tyBinds t_lhs allBinds1 rhs2))
    pure (mkEApp (mkETyApp (EVal lhs) (map TVar tvCaptured)) (map EVar evCaptured))
  ECoe c e0 -> ECoe c <$> llExpr e0
  ETyApp e0 ts -> ETyApp <$> llExpr e0 <*> pure ts
  ETyAbs qvs e0 -> ETyAbs qvs <$> withQVars qvs (llExpr e0)

llCase ::
  (HasEnv tv, IsTVar tv, IsEVar ev) => Case In tv ev -> LL tv ev (Case Out tv ev)
llCase (MkCase dcon ts0 bs e) = do
  (MkTConDecl _ vs _, MkDConDecl _ _ _ flds0) <- findInfo info2dcons dcon
  unless (length vs == length ts0)
    (bug "wrong number of type arguments for type constructor")
  unless (length bs == length flds0) (bug "wrong number of binds in pattern")
  let t_flds = fmap (instantiateN' ts0) flds0
  MkCase dcon ts0 bs <$> withinEScope' id t_flds (llExpr e)

llDecl :: Decl In -> LL Void Void ()
llDecl = \case
  DType ds -> yield (DType ds)
  DDefn (MkDefn (MkBind lhs t) rhs) -> do
    resetWith (Id.freshEVars "ll" (lhs^.lctd))
    rhs <- llExpr rhs
    yield (DSupC (MkSupCDecl lhs [] (fmap absurd t) [] (bimap absurd absurd rhs)))
  DPrim p -> yield (DPrim p)

liftModule :: Module In -> Module Out
liftModule m0@(MkModule tops0) = MkModule (execLL m0 (traverse_ llDecl tops0))
