{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.FrontEnd.Gamma where

import Pukeko.Prelude

import           Pukeko.AST.SystemF
import           Pukeko.AST.Type
import           Pukeko.FrontEnd.Info (MonadInfo)
import qualified Pukeko.AST.Identifier as Id

data GammaEnv tf ef tv ev = GammaEnv
  { _tenv :: EnvOf ev (ef tv)
  , _kenv :: EnvOf tv (tf ev)
  }
makeLenses ''GammaEnv

newtype XGammaT tf ef tv ev m a =
  GammaT{unGammaT :: ReaderT (GammaEnv tf ef tv ev) m a}
  deriving ( Functor, Applicative, Monad, MonadTrans
           , MonadError e, MonadSupply s, MonadWriter w, MonadHere
           )

type GammaT = XGammaT (Const (Set Id.Clss)) Type

runGammaT :: XGammaT tf ef Void Void m a -> m a
runGammaT m = runReaderT (unGammaT m) (GammaEnv voidEnv voidEnv)

mapGammaT :: (m a -> n b) -> XGammaT tf ef tv ev m a -> XGammaT tf ef tv ev n b
mapGammaT f = GammaT . mapReaderT f . unGammaT

withinEScope ::
  forall i m tf ef tv ev a. (HasEnvLevel i, HasEnv tv, HasEnv ev, Functor tf) =>
  EnvLevelOf i (ef tv) -> XGammaT tf ef tv (EScope i ev) m a -> XGammaT tf ef tv ev m a
withinEScope ts = withXGammaT (extendEnv @i @ev ts) (fmap (fmap weaken))

withBinds ::
  (HasEnvLevel i, HasEnv tv, HasEnv ev) =>
  EnvLevelOf i (Bind Type tv) -> GammaT tv (EScope i ev) m a -> GammaT tv ev m a
withBinds = withinEScope . fmap _bind2type

withinTScope ::
  forall i m tf ef tv ev a.
  (HasEnvLevel i, HasEnv tv, HasEnv ev, Functor ef) =>
  EnvLevelOf i (tf ev) -> XGammaT tf ef (TScope i tv) ev m a -> XGammaT tf ef tv ev m a
withinTScope qs = withXGammaT (fmap (fmap weaken)) (extendEnv @i @tv qs)

withQVars ::
  (HasEnvLevel i, HasEnv tv, HasEnv ev) =>
  EnvLevelOf i QVar -> GammaT (TScope i tv) ev m a -> GammaT tv ev m a
withQVars = withinTScope . fmap (Const . _qvar2cstr)

withinXScope ::
  forall i j tf ef tv ev m a.
  (HasEnvLevel i, HasEnvLevel j, HasEnv tv, HasEnv ev, Functor tf, Functor ef) =>
  EnvLevelOf i (tf (EScope j ev)) -> EnvLevelOf j (ef (TScope i tv)) ->
  XGammaT tf ef (TScope i tv) (EScope j ev) m a -> XGammaT tf ef tv ev m a
withinXScope qs ts = withXGammaT
  (extendEnv @j @ev ts . fmap (fmap weaken))
  (extendEnv @i @tv qs . fmap (fmap weaken))

lookupEVar :: (HasEnv ev, Monad m) => ev -> XGammaT tf ef tv ev m (ef tv)
lookupEVar = GammaT . views tenv . lookupEnv

lookupTVar :: (HasEnv tv, Monad m) => tv -> XGammaT tf ef tv ev m (tf ev)
lookupTVar = GammaT . views kenv . lookupEnv

lookupQual :: (HasEnv tv, Monad m) => tv -> GammaT tv ev m (Set Id.Clss)
lookupQual = fmap getConst . lookupTVar

withXGammaT ::
  (EnvOf ev1 (ef1 tv1) -> EnvOf ev2 (ef2 tv2)) ->
  (EnvOf tv1 (tf1 ev1) -> EnvOf tv2 (tf2 ev2)) ->
  XGammaT tf2 ef2 tv2 ev2 m a -> XGammaT tf1 ef1 tv1 ev1 m a
withXGammaT f g = GammaT . withReaderT upd . unGammaT
  where upd = (\(GammaEnv ev_env tv_env) -> GammaEnv (f ev_env) (g tv_env))

deriving instance (MonadInfo m) => MonadInfo (XGammaT tf ef tv ev m)
