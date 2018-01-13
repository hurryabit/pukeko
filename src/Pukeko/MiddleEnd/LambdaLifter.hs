{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
module Pukeko.MiddleEnd.LambdaLifter
  ( Out
  , liftModule
  )
where

import           Control.Lens
import           Control.Monad.Supply
import           Control.Monad.Writer
import qualified Data.Finite       as Fin
import           Data.Foldable     (traverse_)
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import qualified Data.Set.Lens     as Set
import           Data.Traversable  (for)
import qualified Data.Vector.Sized as Vec
import           GHC.TypeLits

import           Pukeko.Error
import           Pukeko.AST.SystemF
import qualified Pukeko.AST.Stage      as St
import qualified Pukeko.AST.ConDecl    as Con
import qualified Pukeko.AST.ModuleInfo as MI
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.FrontEnd.Gamma
import           Pukeko.FrontEnd.Info
import           Pukeko.AST.Type

type In  = St.PatternMatcher
type Out = St.LambdaLifter

type IsTVar tv = (Ord tv, BaseTVar tv)
type IsEVar ev = (Ord ev, BaseEVar ev, HasEnv ev)

type LL tv ev a =
  GammaT tv ev (InfoT (ModuleInfo In) (SupplyT Id.EVar (Writer [TopLevel Out]))) a

execLL :: LL Void Void () -> ModuleInfo In -> [TopLevel Out]
execLL ll info = execWriter (evalSupplyT (runInfoT (runGammaT ll) info) [])

yield :: TopLevel Out -> LL tv ev ()
yield top = tell [top]

llExpr ::
  forall tv ev. (IsTVar tv, IsEVar ev) =>
  Expr In tv ev -> LL tv ev (Expr Out tv ev)
llExpr = \case
  EVar w x -> pure (EVar w x)
  EVal w z -> pure (EVal w z)
  ECon w c -> pure (ECon w c)
  ENum w n -> pure (ENum w n)
  EApp w t  us -> EApp w <$> llExpr t <*> traverse llExpr us
  ECas w t  cs -> ECas w <$> llExpr t <*> traverse llCase cs
  ELet w ds e0 ->
    ELet w
    <$> (traverse . defn2rhs) llExpr ds
    <*> withBinds (fmap _defnLhs ds) (llExpr e0)
  ERec w ds e0 ->
    withBinds (fmap _defnLhs ds) $
      ERec w <$> (traverse . defn2rhs) llExpr ds <*> llExpr e0
  ELam w (oldBinds :: Vec.Vector n1 (Bind In tv)) rhs0 (t_rhs :: Type tv) -> do
    rhs1 :: Expr Out tv (EFinScope n1 ev) <- withBinds oldBinds (llExpr rhs0)
    let evCapturedL :: [ev]
        evCapturedL = Set.toList (Set.setOf (traverse . _Free) rhs1)
    Vec.withList evCapturedL $ \(evCapturedV :: Vec.Vector n0 ev) -> do
      newBinds :: Vec.Vector n0 (Bind Out tv) <-
        for evCapturedV $ \x -> MkBind w (baseEVar x) <$> lookupType x
      let allBinds0 :: Vec.Vector (n0 + n1) (Bind Out tv)
          allBinds0 = newBinds Vec.++ fmap retagBind oldBinds
      let tvCapturedL :: [tv]
          tvCapturedL = Set.toList (Set.setOf (traverse . bindType . traverse) allBinds0)
      Vec.withList tvCapturedL $ \(tvCapturedV :: Vec.Vector m tv) -> do
        let tyBinds :: Vec.Vector m Id.TVar
            tyBinds = fmap baseTVar tvCapturedV
        let tvMap :: Map.Map tv (TFinScope m Void)
            tvMap =
              ifoldMap (\j v -> Map.singleton v (mkBound j (baseTVar v))) tvCapturedV
        let tvRename :: tv -> TFinScope m Void
            tvRename = (tvMap Map.!)
        let evMap :: Map.Map ev (EFinScope (n0 + n1) Void)
            evMap =
              ifoldMap
              (\i x -> Map.singleton x (mkBound (Fin.weaken i) (baseEVar x)))
              evCapturedV
        let evRename :: EFinScope n1 ev -> EFinScope (n0 + n1) Void
            evRename = scope' (evMap Map.!) (mkBound . Fin.shift)
        let allBinds1 :: Vec.Vector (n0 + n1) (Bind Out (TFinScope m Void))
            allBinds1 = fmap (fmap tvRename) allBinds0
        let rhs2 :: Expr Out (TFinScope m Void) (EFinScope (n0 + n1) Void)
            rhs2 = bimap tvRename evRename rhs1
        lhs <- fresh
        let t_lhs :: Type (TFinScope m Void)
            t_lhs = fmap _bindType allBinds1 *~> fmap tvRename t_rhs
        yield (TLSup w lhs tyBinds t_lhs allBinds1 rhs2)
        pure
          (mkEApp w
            (mkETyApp w
              (EVal w lhs)
              (map TVar tvCapturedL))
            (map (EVar w) evCapturedL))
  ETyApp w e0 ts -> ETyApp w <$> llExpr e0 <*> pure ts
  ETyAbs w vs e0 -> ETyAbs w vs <$> withKinds (llExpr e0)

llCase :: (IsTVar tv, IsEVar ev) => Case In tv ev -> LL tv ev (Case Out tv ev)
llCase (MkCase w dcon ts0 bs e) = do
  Some1 (Pair1 (Con.MkTConDecl _ vs _) (Con.MkDConDecl _ _ _ flds0)) <- findDCon dcon
  case Vec.matchList vs ts0 of
    Nothing  -> bug "wrong number of type arguments for type constructor"
    Just ts1 ->
      case Vec.matchList bs flds0 of
        Nothing    -> bug "wrong number of binds in pattern"
        Just flds1 -> do
          let t_flds = fmap (>>= scope absurd (ts1 Vec.!)) flds1
          MkCase w dcon ts0 bs <$> withTypes t_flds (llExpr e)

llTopLevel :: TopLevel In -> LL Void Void ()
llTopLevel = \case
  TLDef (MkDefn (MkBind w lhs t) rhs) -> do
    resetWith (Id.freshEVars "ll" lhs)
    rhs <- llExpr rhs
    yield (TLSup w lhs Vec.empty (fmap absurd t) Vec.empty (bimap absurd absurd rhs))
  TLAsm b asm -> yield (TLAsm (retagBind b :: Bind Out Void) asm)

collectFuns :: [TopLevel Out] -> Map.Map Id.EVar (Pos, Type Void)
collectFuns = foldMap $ \case
  TLSup w z vs t _ _ -> Map.singleton z (w, mkTUni vs t)
  TLAsm (MkBind w z t) _ -> Map.singleton z (w, t)

liftModule :: Module In -> Module Out
liftModule (MkModule info0 tops0) =
  let tops1 = execLL (traverse_ llTopLevel tops0) info0
      info1 = set MI.info2funs (collectFuns tops1) info0
  in  MkModule info1 tops1
