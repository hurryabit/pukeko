module Pukeko.MiddleEnd.LambdaLifter
  ( liftModule
  )
where

import Pukeko.Prelude

import           Control.Monad.Freer.Supply
import qualified Data.Map          as Map
import qualified Data.Set          as Set

import           Pukeko.AST.SystemF
import           Pukeko.AST.SuperCore hiding (Module (..), Expr, Defn, Case, Bind)
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

llExpr ::
  forall tv ev. (HasEnv tv, IsTVar tv, IsEVar ev) =>
  Expr In tv ev -> LL tv ev (Expr Out tv ev)
llExpr = \case
  ELoc le -> here le $ llExpr (unlctd le)
  EVar x -> pure (EVar x)
  EAtm a -> pure (EAtm a)
  EApp t  us -> EApp <$> llExpr t <*> traverse llExpr us
  ECas t  cs -> ECas <$> llExpr t <*> traverse llCase cs
  ELet ds e0 ->
    ELet
    <$> (traverse . defn2expr) llExpr ds
    <*> withinEScope' (_bind2type . _defn2bind) ds (llExpr e0)
  ERec ds e0 ->
    withinEScope' (_bind2type . _defn2bind) ds $
      ERec <$> (traverse . defn2expr) llExpr ds <*> llExpr e0
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
    let t_lhs = mkTUni tyBinds (map _bind2type allBinds1 *~> fmap tvRename t_rhs)
    -- TODO: We could use the pos of the lambda for @lhs@.
    let supc = SupCDecl (MkBind (Lctd noPos lhs) t_lhs) tyBinds allBinds1 rhs2
    tell (mkFuncDecl @'SupC supc)
    pure (mkEApp (mkETyApp (EVal lhs) (map TVar tvCaptured)) (map EVar evCaptured))
  ETyCoe c e0 -> ETyCoe c <$> llExpr e0
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
  DType ds -> tell (foldMap mkTypeDecl ds)
  DDefn (MkDefn (MkBind lhs t) rhs) -> do
    resetWith (Id.freshEVars "ll" (lhs^.lctd))
    rhs <- llExpr rhs
    let supc = SupCDecl (MkBind lhs (fmap absurd t)) [] [] (bimap absurd absurd rhs)
    tell (mkFuncDecl @'SupC supc)
  DExtn (MkExtnDecl z s) -> tell (mkFuncDecl @'Extn (ExtnDecl z s))

liftModule :: Module In -> Core.Module
liftModule m0@(MkModule decls) = execLL m0 (traverse_ llDecl decls)
