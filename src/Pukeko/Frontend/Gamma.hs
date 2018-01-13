{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.FrontEnd.Gamma where

import           Control.Monad.Error.Class
import           Control.Monad.Reader
import           Control.Monad.Supply
import           Control.Monad.Writer.Class
import           Data.Functor.Const

import           Pukeko.AST.SystemF
import           Pukeko.AST.Stage     (StageType)
import           Pukeko.FrontEnd.Info (MonadInfo)
import           Pukeko.AST.Type

type GammaEnv tv ev = EnvOf ev (Type tv)

newtype GammaT tv ev m a = GammaT{unGammaT :: ReaderT (GammaEnv tv ev) m a}
  deriving ( Functor, Applicative, Monad, MonadTrans
           , MonadError e, MonadSupply s, MonadWriter w
           )

deriving instance (MonadInfo i m) => MonadInfo i (GammaT tv ev m)

runGammaT :: GammaT Void Void m a -> m a
runGammaT m = runReaderT (unGammaT m) (Const ())

withTypes ::
  forall i m tv ev a. (HasEnvLevel i, HasEnv ev) =>
  EnvLevelOf i (Type tv) -> GammaT tv (EScope i ev) m a -> GammaT tv ev m a
withTypes ts = GammaT . withReaderT (extendEnv @i @ev ts) . unGammaT

withBinds ::
  (StageType st ~ Type, HasEnvLevel i, HasEnv ev) =>
  EnvLevelOf i (Bind st tv) -> GammaT tv (EScope i ev) m a -> GammaT tv ev m a
withBinds = withTypes . fmap _bindType

withKinds :: (HasEnv ev) => GammaT (TFinScope i tv) ev m a -> GammaT tv ev m a
withKinds = GammaT . withReaderT (fmap (fmap weaken)) . unGammaT

lookupType :: (HasEnv ev, Monad m) => ev -> GammaT tv ev m (Type tv)
lookupType = GammaT . asks . lookupEnv
