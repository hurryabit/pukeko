{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Pukeko.MiddleEnd.EtaReducer
  ( reduceModule
  ) where

import Pukeko.Prelude

import           Control.Bilens     (firstOf)
import           Data.Bitraversable
import           Data.Finite        (absurd0)
import qualified Data.Vector.Sized  as Vec

import           Pukeko.AST.SystemF
import           Pukeko.AST.Stage   (StageType)
import           Pukeko.AST.Type

reduceModule :: Module st -> Module st
reduceModule = over (module2decls . traverse) erDecl

_EVar :: Expr st tv ev -> Maybe ev
_EVar = \case
  EVar _ x -> Just x
  _        -> Nothing

_TVar :: Type tv -> Maybe tv
_TVar = \case
  TVar v -> Just v
  _      -> Nothing

erDecl :: forall st. Decl st -> Decl st
erDecl top = case top of
  DType{} -> top
  DSign{} -> top
  DPrim{} -> top
  DDefn{} -> top
  DSupC (MkSupCDecl w z
    (vs0 :: Vector m0 QVar)
    (t0 :: Type (TFinScope m0 Void))
    (bs0 :: Vector n0 (Bind (StageType st) (TFinScope m0 Void)))
    (e0 :: Expr st (TFinScope m0 Void) (EFinScope n0 Void))) ->
    case e0 of
      EApp _ (traverse strengthen -> Just e1) (traverse _EVar -> Just xs1)
        | Just xs2 <- Vec.matchNonEmpty bs0 xs1
        , iall (\i x -> i == scope absurd id x) xs2 ->
          erDecl (DSupC (MkSupCDecl w z vs0 t0 Vec.empty (fmap weaken e1)))
      ETyApp _
        (bitraverse strengthen strengthen -> Just e1)
        (traverse _TVar -> Just vs1)
        | null bs0
        , Just vs2 <- Vec.matchNonEmpty vs0 vs1
        , iall (\i v -> i == scope absurd id v) vs2 ->
          DSupC (MkSupCDecl w z
            Vec.empty (fmap weaken (mkTUni vs0 t0))
            Vec.empty (bimap weaken weaken e1))
      ETyAbs _
        (_ :: Vector m1 QVar)
        (e1 :: Expr st (TFinScope m1 (TFinScope m0 Void)) (EFinScope n0 Void))
        | Just Refl <- sameNat (Proxy @m0) (Proxy @0)
        , TUni (vs2 :: Vector m2 QVar) (t2 :: Type (TFinScope m2 Void)) <-
            fmap (strengthenWith absurd0) t0
        , Just Refl <- sameNat (Proxy @m1) (Proxy @m2) ->
            let bs2 :: Vector n0 (Bind (StageType st) (TFinScope m2 Void))
                bs2 = fmap (fmap (first absurd0)) bs0
                e2, e3 :: Expr st (TFinScope m2 Void) (EFinScope n0 Void)
                e2 = first (fmap (strengthenWith absurd0)) e1
                e3 = over
                     (firstOf bitraverse . _Bound)
                     (\(i, _) -> (i, (vs2 Vec.! i)^.qvar2tvar)) e2
            in  erDecl (DSupC (MkSupCDecl w z vs2 t2 bs2 e3))
      _ -> top
