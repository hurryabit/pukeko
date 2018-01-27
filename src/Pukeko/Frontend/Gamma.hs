module Pukeko.FrontEnd.Gamma where

import Pukeko.Prelude

import           Pukeko.AST.SystemF
import           Pukeko.AST.Type
import qualified Pukeko.AST.Identifier as Id

data Gamma tf ef tv ev = Gamma
  { _tenv :: EnvOf ev (ef tv)
  , _kenv :: EnvOf tv (tf ev)
  }
makeLenses ''Gamma

type EffXGamma tf ef tv ev effs = Eff (Reader (Gamma tf ef tv ev) : effs)

type EffGamma tv ev effs = EffXGamma (Const (Set Id.Clss)) Type tv ev effs

runGamma :: EffXGamma tf ef Void Void effs a -> Eff effs a
runGamma = runReader (Gamma voidEnv voidEnv)

withinEScope ::
  forall i tf ef tv ev effs a. (HasEnvLevel i, HasEnv tv, HasEnv ev, Functor tf) =>
  EnvLevelOf i (ef tv) ->
  EffXGamma tf ef tv (EScope i ev) effs a ->
  EffXGamma tf ef tv ev effs a
withinEScope ts = localX (extendEnv @i @ev ts) (fmap (fmap weaken))

withBinds ::
  (HasEnvLevel i, HasEnv tv, HasEnv ev) =>
  EnvLevelOf i (Bind Type tv) -> EffGamma tv (EScope i ev) m a -> EffGamma tv ev m a
withBinds = withinEScope . fmap _bind2type

withinTScope ::
  forall i tf ef tv ev effs a.
  (HasEnvLevel i, HasEnv tv, HasEnv ev, Functor ef) =>
  EnvLevelOf i (tf ev) ->
  EffXGamma tf ef (TScope i tv) ev effs a ->
  EffXGamma tf ef tv ev effs a
withinTScope qs = localX (fmap (fmap weaken)) (extendEnv @i @tv qs)

withQVars ::
  (HasEnvLevel i, HasEnv tv, HasEnv ev) =>
  EnvLevelOf i QVar -> EffGamma (TScope i tv) ev effs a -> EffGamma tv ev effs a
withQVars = withinTScope . fmap (Const . _qvar2cstr)

withinXScope ::
  forall i j tf ef tv ev effs a.
  (HasEnvLevel i, HasEnvLevel j, HasEnv tv, HasEnv ev, Functor tf, Functor ef) =>
  EnvLevelOf i (tf (EScope j ev)) -> EnvLevelOf j (ef (TScope i tv)) ->
  EffXGamma tf ef (TScope i tv) (EScope j ev) effs a -> EffXGamma tf ef tv ev effs a
withinXScope qs ts = localX
  (extendEnv @j @ev ts . fmap (fmap weaken))
  (extendEnv @i @tv qs . fmap (fmap weaken))

lookupEVar ::
  forall tf ef tv ev effs. (HasEnv ev) => ev -> EffXGamma tf ef tv ev effs (ef tv)
lookupEVar = views (tenv @tf @ef @tv @ev) . lookupEnv

lookupTVar ::
  forall tf ef tv ev effs. (HasEnv tv) => tv -> EffXGamma tf ef tv ev effs (tf ev)
lookupTVar = views (kenv @tf @ef @tv @ev) . lookupEnv

lookupQual :: (HasEnv tv) => tv -> EffGamma tv ev effs (Set Id.Clss)
lookupQual = fmap getConst . lookupTVar

localX ::
  (EnvOf ev1 (ef1 tv1) -> EnvOf ev2 (ef2 tv2)) ->
  (EnvOf tv1 (tf1 ev1) -> EnvOf tv2 (tf2 ev2)) ->
  EffXGamma tf2 ef2 tv2 ev2 effs a -> EffXGamma tf1 ef1 tv1 ev1 effs a
localX f g = local' upd
  where upd = (\(Gamma ev_env tv_env) -> Gamma (f ev_env) (g tv_env))
