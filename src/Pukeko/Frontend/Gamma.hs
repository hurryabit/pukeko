{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.FrontEnd.Gamma where

import Pukeko.Prelude

import           Pukeko.AST.SystemF
import           Pukeko.AST.Type
import           Pukeko.FrontEnd.Info (MonadInfo)
import qualified Pukeko.AST.Identifier as Id

data GammaEnv tv ev = GammaEnv
  { _tenv :: EnvOf ev (Type tv)
  , _kenv :: EnvOf tv (Set Id.Clss)
  }
makeLenses ''GammaEnv

newtype GammaT tv ev m a = GammaT{unGammaT :: ReaderT (GammaEnv tv ev) m a}
  deriving ( Functor, Applicative, Monad, MonadTrans
           , MonadError e, MonadSupply s, MonadWriter w
           )

deriving instance (MonadInfo m) => MonadInfo (GammaT tv ev m)

runGammaT :: GammaT Void Void m a -> m a
runGammaT m = runReaderT (unGammaT m) (GammaEnv (Const ()) (Const ()))

withTypes ::
  forall i m tv ev a. (HasEnvLevel i, HasEnv ev) =>
  EnvLevelOf i (Type tv) -> GammaT tv (EScope i ev) m a -> GammaT tv ev m a
withTypes ts = GammaT . withReaderT (tenv %~ extendEnv @i @ev ts) . unGammaT

withBinds ::
  (HasEnvLevel i, HasEnv ev) =>
  EnvLevelOf i (Bind Type tv) -> GammaT tv (EScope i ev) m a -> GammaT tv ev m a
withBinds = withTypes . fmap _bind2type

withKinds ::
  forall i m tv ev a.
  (HasEnvLevel i, HasEnv tv, HasEnv ev) =>
  EnvLevelOf i (Set Id.Clss) -> GammaT (TScope i tv) ev m a -> GammaT tv ev m a
withKinds qs = GammaT . withReaderT upd . unGammaT
  where
    upd (GammaEnv t_env k_env) =
      GammaEnv (fmap (fmap weaken) t_env) (extendEnv @i @tv qs k_env)

withQVars ::
  (HasEnvLevel i, HasEnv tv, HasEnv ev) =>
  EnvLevelOf i QVar -> GammaT (TScope i tv) ev m a -> GammaT tv ev m a
withQVars = withKinds . fmap _qvar2cstr

lookupType :: (HasEnv ev, Monad m) => ev -> GammaT tv ev m (Type tv)
lookupType = GammaT . views tenv . lookupEnv

lookupKind :: (HasEnv tv, Monad m) => tv -> GammaT tv ev m (Set Id.Clss)
lookupKind = GammaT . views kenv . lookupEnv
