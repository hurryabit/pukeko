module Pukeko.FrontEnd.Gamma where

import Pukeko.Prelude

import qualified Data.Vector as Vec

import           Pukeko.AST.Name
import           Pukeko.AST.SystemF
import           Pukeko.AST.Type

data Gamma tf ef tv ev = Gamma
  { _tenv :: EnvOf ev (ef tv)
  , _kenv :: EnvOf tv (tf ev)
  }
makeLenses ''Gamma

type EffXGamma tf ef tv ev effs = Eff (Reader (Gamma tf ef tv ev) : effs)

type EffGamma tv ev effs = EffXGamma (Const (Set (Name Clss))) Type tv ev effs

runGamma :: EffXGamma tf ef Void Void effs a -> Eff effs a
runGamma = runReader (Gamma voidEnv voidEnv)

withinEScope ::
  forall i tf ef tv ev effs a x. (HasEnvLevel i, HasEnv tv, HasEnv ev, Functor tf) =>
  (x -> ef tv) ->
  EnvLevelOf i x ->
  EffXGamma tf ef tv (EScope i ev) effs a ->
  EffXGamma tf ef tv ev effs a
withinEScope f ts = localX (extendEnv @i @ev (fmap f ts)) (fmap (fmap weakenScope))

withinEScope' ::
  forall tf ef tv ev t effs a x. (HasEnv tv, HasEnv ev, Functor tf, Foldable t) =>
  (x -> ef tv) ->
  t x ->
  EffXGamma tf ef tv (EScope Int ev) effs a ->
  EffXGamma tf ef tv ev effs a
withinEScope' f = withinEScope f . Vec.fromList . toList

withinEScope1 ::
  forall tf ef tv ev effs a x. (HasEnv tv, HasEnv ev, Functor tf) =>
  (x -> ef tv) ->
  x ->
  EffXGamma tf ef tv (EScope () ev) effs a ->
  EffXGamma tf ef tv ev effs a
withinEScope1 f = withinEScope f . Identity

withinTScope ::
  forall i tf ef tv ev effs a.
  (HasEnvLevel i, HasEnv tv, HasEnv ev, Functor ef) =>
  EnvLevelOf i (tf ev) ->
  EffXGamma tf ef (TScope i tv) ev effs a ->
  EffXGamma tf ef tv ev effs a
withinTScope qs = localX (fmap (fmap weakenScope)) (extendEnv @i @tv qs)

withQVars ::
  (HasEnv tv, HasEnv ev, Foldable t) =>
  t QVar -> EffGamma (TScope Int tv) ev effs a -> EffGamma tv ev effs a
withQVars = withinTScope . Vec.fromList . map (Const . _qvar2cstr) . toList

withinXScope ::
  forall i j tf ef tv ev effs a.
  (HasEnvLevel i, HasEnvLevel j, HasEnv tv, HasEnv ev, Functor tf, Functor ef) =>
  EnvLevelOf i (tf (EScope j ev)) -> EnvLevelOf j (ef (TScope i tv)) ->
  EffXGamma tf ef (TScope i tv) (EScope j ev) effs a -> EffXGamma tf ef tv ev effs a
withinXScope qs ts = localX
  (extendEnv @j @ev ts . fmap (fmap weakenScope))
  (extendEnv @i @tv qs . fmap (fmap weakenScope))

lookupEVar ::
  forall tf ef tv ev effs. (HasEnv ev) => ev -> EffXGamma tf ef tv ev effs (ef tv)
lookupEVar = views (tenv @tf @ef @tv @ev) . lookupEnv

lookupTVar ::
  forall tf ef tv ev effs. (HasEnv tv) => tv -> EffXGamma tf ef tv ev effs (tf ev)
lookupTVar = views (kenv @tf @ef @tv @ev) . lookupEnv

lookupQual :: (HasEnv tv) => tv -> EffGamma tv ev effs (Set (Name Clss))
lookupQual = fmap getConst . lookupTVar

localX ::
  (EnvOf ev1 (ef1 tv1) -> EnvOf ev2 (ef2 tv2)) ->
  (EnvOf tv1 (tf1 ev1) -> EnvOf tv2 (tf2 ev2)) ->
  EffXGamma tf2 ef2 tv2 ev2 effs a -> EffXGamma tf1 ef1 tv1 ev1 effs a
localX f g = local' upd
  where upd = (\(Gamma ev_env tv_env) -> Gamma (f ev_env) (g tv_env))
