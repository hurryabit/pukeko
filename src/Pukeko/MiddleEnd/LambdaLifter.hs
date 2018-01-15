{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
module Pukeko.MiddleEnd.LambdaLifter
  ( Out
  , liftModule
  )
where

import Pukeko.Prelude

import qualified Data.Finite       as Fin
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import qualified Data.Vector.Sized as Vec

import           Pukeko.AST.SystemF
import qualified Pukeko.AST.Stage      as St
import           Pukeko.AST.ConDecl
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.FrontEnd.Gamma
import           Pukeko.FrontEnd.Info
import           Pukeko.AST.Type

type In  = St.PatternMatcher
type Out = St.LambdaLifter

type IsTVar tv = (Ord tv, BaseTVar tv)
type IsEVar ev = (Ord ev, BaseEVar ev, HasEnv ev)

type LL tv ev a =
  GammaT tv ev (InfoT (SupplyT Id.EVar (Writer [Decl Out]))) a

execLL :: LL Void Void () -> Module In -> [Decl Out]
execLL ll m0 = execWriter (evalSupplyT (runInfoT (runGammaT ll) m0) [])

yield :: Decl Out -> LL tv ev ()
yield top = tell [top]

llExpr ::
  forall tv ev. (HasEnv tv, IsTVar tv, IsEVar ev) =>
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
    <$> (traverse . defn2exprSt) llExpr ds
    <*> withBinds (fmap _defn2bind ds) (llExpr e0)
  ERec w ds e0 ->
    withBinds (fmap _defn2bind ds) $
      ERec w <$> (traverse . defn2exprSt) llExpr ds <*> llExpr e0
  ELam w (oldBinds :: Vector n1 (Bind Type tv)) rhs0 (t_rhs :: Type tv) -> do
    rhs1 :: Expr Out tv (EFinScope n1 ev) <- withBinds oldBinds (llExpr rhs0)
    let evCapturedL :: [ev]
        evCapturedL = Set.toList (setOf (traverse . _Free) rhs1)
    Vec.withList evCapturedL $ \(evCapturedV :: Vector n0 ev) -> do
      newBinds :: Vector n0 (Bind Type tv) <-
        for evCapturedV $ \x -> MkBind w (baseEVar x) <$> lookupType x
      let allBinds0 :: Vector (n0 + n1) (Bind Type tv)
          allBinds0 = newBinds Vec.++ oldBinds
      let tvCapturedL :: [tv]
          tvCapturedL = Set.toList (setOf (traverse . bind2type . traverse) allBinds0)
      Vec.withList tvCapturedL $ \(tvCapturedV :: Vector m tv) -> do
        tyBinds :: Vector m QVar <-
          traverse (\v -> MkQVar <$> lookupKind v <*> pure (baseTVar v)) tvCapturedV
        let tvMap :: Map tv (TFinScope m Void)
            tvMap =
              ifoldMap (\j v -> Map.singleton v (mkBound j (baseTVar v))) tvCapturedV
        let tvRename :: tv -> TFinScope m Void
            tvRename = (tvMap Map.!)
        let evMap :: Map ev (EFinScope (n0 + n1) Void)
            evMap =
              ifoldMap
              (\i x -> Map.singleton x (mkBound (Fin.weaken i) (baseEVar x)))
              evCapturedV
        let evRename :: EFinScope n1 ev -> EFinScope (n0 + n1) Void
            evRename = scope' (evMap Map.!) (mkBound . Fin.shift)
        let allBinds1 :: Vector (n0 + n1) (Bind Type (TFinScope m Void))
            allBinds1 = fmap (fmap tvRename) allBinds0
        let rhs2 :: Expr Out (TFinScope m Void) (EFinScope (n0 + n1) Void)
            rhs2 = bimap tvRename evRename rhs1
        lhs <- fresh
        let t_lhs :: Type (TFinScope m Void)
            t_lhs = fmap _bind2type allBinds1 *~> fmap tvRename t_rhs
        yield (DSupC (MkSupCDecl w lhs tyBinds t_lhs allBinds1 rhs2))
        pure
          (mkEApp w
            (mkETyApp w
              (EVal w lhs)
              (map TVar tvCapturedL))
            (map (EVar w) evCapturedL))
  ETyApp w e0 ts -> ETyApp w <$> llExpr e0 <*> pure ts
  ETyAbs w qvs e0 -> ETyAbs w qvs <$> withQVars qvs (llExpr e0)

llCase :: (HasEnv tv, IsTVar tv, IsEVar ev) => Case In tv ev -> LL tv ev (Case Out tv ev)
llCase (MkCase w dcon ts0 bs e) = do
  Some1 (Pair1 (MkTConDecl _ _ vs _) (MkDConDecl _ _ _ _ flds0)) <- findDCon dcon
  case Vec.matchList vs ts0 of
    Nothing  -> bug "wrong number of type arguments for type constructor"
    Just ts1 ->
      case Vec.matchList bs flds0 of
        Nothing    -> bug "wrong number of binds in pattern"
        Just flds1 -> do
          let t_flds = fmap (>>= scope absurd (ts1 Vec.!)) flds1
          MkCase w dcon ts0 bs <$> withTypes t_flds (llExpr e)

llDecl :: Decl In -> LL Void Void ()
llDecl = \case
  DType ds -> yield (DType ds)
  -- FIXME: Erase type classes when converting to dictionary passing style.
  DClss _ -> pure ()
  DInst _ -> pure ()
  DDefn (MkDefn (MkBind w lhs t) rhs) -> do
    resetWith (Id.freshEVars "ll" lhs)
    rhs <- llExpr rhs
    yield (DSupC (MkSupCDecl w lhs
                  Vec.empty (fmap absurd t)
                  Vec.empty (bimap absurd absurd rhs)))
  DPrim p -> yield (DPrim p)

liftModule :: Module In -> Module Out
liftModule m0@(MkModule tops0) =
  let tops1 = execLL (traverse_ llDecl tops0) m0
  in  MkModule tops1
