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

type In  = St.ClassEliminator
type Out = St.LambdaLifter

type IsTVar tv = (Ord tv, BaseTVar tv)
type IsEVar ev = (Ord ev, BaseEVar ev, HasEnv ev)

type LL tv ev a =
  GammaT tv ev (InfoT (HereT (SupplyT Id.EVar (Writer [Loc (Decl Out)])))) a

execLL :: Module In -> LL Void Void () -> [Loc (Decl Out)]
execLL m0 ll = execWriter (evalSupplyT (runHereT (runInfoT (runGammaT ll) m0)) [])

yield :: Decl Out -> LL tv ev ()
yield decl = do
  pos <- where_
  tell [Loc pos decl]

llExpr ::
  forall tv ev. (HasEnv tv, IsTVar tv, IsEVar ev) =>
  Expr In tv ev -> LL tv ev (Expr Out tv ev)
llExpr = \case
  ELoc l -> ELoc <$> traverseHere llExpr l
  EVar x -> pure (EVar x)
  EVal z -> pure (EVal z)
  ECon c -> pure (ECon c)
  ENum n -> pure (ENum n)
  EApp t  us -> EApp <$> llExpr t <*> traverse llExpr us
  ECas t  cs -> ECas <$> llExpr t <*> traverse llCase cs
  ELet ds e0 ->
    ELet
    <$> (traverseHeres . defn2exprSt) llExpr ds
    <*> withBinds (fmap (_defn2bind . unloc) ds) (llExpr e0)
  ERec ds e0 ->
    withBinds (fmap (_defn2bind . unloc) ds) $
      ERec <$> (traverseHeres . defn2exprSt) llExpr ds <*> llExpr e0
  ELam (oldBinds :: Vector n1 (Bind Type tv)) rhs0 (t_rhs :: Type tv) -> do
    rhs1 :: Expr Out tv (EFinScope n1 ev) <- withBinds oldBinds (llExpr rhs0)
    let evCapturedL :: [ev]
        evCapturedL = Set.toList (setOf (traverse . _Free) rhs1)
    Vec.withList evCapturedL $ \(evCapturedV :: Vector n0 ev) -> do
      newBinds :: Vector n0 (Bind Type tv) <-
        for evCapturedV $ \x -> MkBind (baseEVar x) <$> lookupEVar x
      let allBinds0 :: Vector (n0 + n1) (Bind Type tv)
          allBinds0 = newBinds Vec.++ oldBinds
      let tvCapturedL :: [tv]
          tvCapturedL = Set.toList
            (setOf (traverse . bind2type . traverse) allBinds0 <> setOf traverse t_rhs)
      Vec.withList tvCapturedL $ \(tvCapturedV :: Vector m tv) -> do
        tyBinds :: Vector m QVar <-
          traverse (\v -> MkQVar <$> lookupQual v <*> pure (baseTVar v)) tvCapturedV
        let tvMap :: Map tv (TFinScope m Void)
            tvMap =
              ifoldMap (\j v -> Map.singleton v (mkBound j (baseTVar v))) tvCapturedV
        let tvRename :: tv -> TFinScope m Void
            tvRename v = Map.findWithDefault (bugWith "tvRename" (baseTVar v)) v tvMap
        let evMap :: Map ev (EFinScope (n0 + n1) Void)
            evMap =
              ifoldMap
              (\i x -> Map.singleton x (mkBound (Fin.weaken i) (baseEVar x)))
              evCapturedV
        let evRename :: EFinScope n1 ev -> EFinScope (n0 + n1) Void
            evRename = scope'
                       (\x -> Map.findWithDefault
                              (bugWith "evRename" (baseEVar x)) x evMap)
                       (mkBound . Fin.shift)
        let allBinds1 :: Vector (n0 + n1) (Bind Type (TFinScope m Void))
            allBinds1 = fmap (fmap tvRename) allBinds0
        let rhs2 :: Expr Out (TFinScope m Void) (EFinScope (n0 + n1) Void)
            rhs2 = bimap tvRename evRename rhs1
        lhs <- fresh
        let t_lhs :: Type (TFinScope m Void)
            t_lhs = fmap _bind2type allBinds1 *~> fmap tvRename t_rhs
        yield (DSupC (MkSupCDecl lhs tyBinds t_lhs allBinds1 rhs2))
        pure
          (mkEApp (mkETyApp (EVal lhs) (map TVar tvCapturedL)) (map EVar evCapturedL))
  ETyApp e0 ts -> ETyApp <$> llExpr e0 <*> pure ts
  ETyAbs qvs e0 -> ETyAbs qvs <$> withQVars qvs (llExpr e0)

llCase ::
  (HasEnv tv, IsTVar tv, IsEVar ev) => Case In tv ev -> LL tv ev (Case Out tv ev)
llCase (MkCase dcon ts0 bs e) = do
  Some1 (Pair1 (MkTConDecl _ vs _) (MkDConDecl _ _ _ flds0)) <-
    findInfo info2dcons dcon
  case Vec.matchList vs ts0 of
    Nothing  -> bug "wrong number of type arguments for type constructor"
    Just ts1 ->
      case Vec.matchList bs flds0 of
        Nothing    -> bug "wrong number of binds in pattern"
        Just flds1 -> do
          let t_flds = fmap (>>= scope absurd (ts1 Vec.!)) flds1
          MkCase dcon ts0 bs <$> withinEScope t_flds (llExpr e)

llDecl :: Decl In -> LL Void Void ()
llDecl = \case
  DType ds -> yield (DType ds)
  -- FIXME: Erase type classes when converting to dictionary passing style.
  DDefn (MkDefn (MkBind lhs t) rhs) -> do
    resetWith (Id.freshEVars "ll" lhs)
    rhs <- llExpr rhs
    yield (DSupC (MkSupCDecl lhs
                  Vec.empty (fmap absurd t)
                  Vec.empty (bimap absurd absurd rhs)))
  DPrim p -> yield (DPrim p)

liftModule :: Module In -> Module Out
liftModule m0@(MkModule tops0) =
  let tops1 = execLL m0 (traverse_ (foldHere llDecl) tops0)
  in  MkModule tops1
