{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Pukeko.Language.EtaReducer
  ( reduceModule
  ) where

import           Control.Lens       hiding (firstOf)
import           Control.Bilens     (firstOf)
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Finite        (absurd0)
import           Data.Proxy
import           Data.Type.Equality
import qualified Data.Vector.Sized as Vec
import           GHC.TypeLits

import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.Ident as Id
import           Pukeko.Language.Type

reduceModule :: Module st -> Module st
reduceModule = over (module2tops . traverse) erTopLevel

_EVar :: Expr st tv ev -> Maybe ev
_EVar = \case
  EVar _ x -> Just x
  _        -> Nothing

_TVar :: Type tv -> Maybe tv
_TVar = \case
  TVar v -> Just v
  _      -> Nothing

erTopLevel :: forall st. TopLevel st -> TopLevel st
erTopLevel top = case top of
  TLTyp{} -> top
  TLVal{} -> top
  TLAsm{} -> top
  TLDef{} -> top
  TLSup w z
    (vs0 :: Vec.Vector m0 Id.TVar)
    (t0 :: Type (TFinScope m0 Void))
    (bs0 :: Vec.Vector n0 (Bind st (TFinScope m0 Void)))
    (e0 :: Expr st (TFinScope m0 Void) (EFinScope n0 Void)) ->
    case e0 of
      EApp _ (traverse strengthen -> Just e1) (traverse _EVar -> Just xs1)
        | Just xs2 <- Vec.matchList bs0 xs1
        , iall (\i x -> i == scope id absurd x) xs2 ->
          erTopLevel (TLSup w z vs0 t0 Vec.empty (fmap weaken e1))
      ETyApp _
        (bitraverse strengthen strengthen -> Just e1)
        (traverse _TVar -> Just vs1)
        | null bs0
        , Just vs2 <- Vec.matchList vs0 vs1
        , iall (\i v -> i == scope id absurd v) vs2 ->
          TLSup w z
            Vec.empty (fmap weaken (mkTUni vs0 t0))
            Vec.empty (bimap weaken weaken e1)
      ETyAbs _
        (_ :: Vec.Vector m1 Id.TVar)
        (e1 :: Expr st (TFinScope m1 (TFinScope m0 Void)) (EFinScope n0 Void))
        | Just Refl <- sameNat (Proxy @m0) (Proxy @0)
        , TUni (vs2 :: Vec.Vector m2 Id.TVar) (t2 :: Type (TFinScope m2 Void)) <-
            fmap (strengthenWith absurd0) t0
        , Just Refl <- sameNat (Proxy @m1) (Proxy @m2) ->
            let bs2 :: Vec.Vector n0 (Bind st (TFinScope m2 Void))
                bs2 = fmap (fmap (first absurd0)) bs0
                e2, e3 :: Expr st (TFinScope m2 Void) (EFinScope n0 Void)
                e2 = first (fmap (strengthenWith absurd0)) e1
                e3 = over (firstOf bitraverse . _Bound) (\(i, _) -> (i, vs2 Vec.! i)) e2
            in  erTopLevel (TLSup w z vs2 t2 bs2 e3)
      _ -> top
