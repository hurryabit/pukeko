{-# LANGUAGE TypeApplications #-}
module Pukeko.FrontEnd.Inferencer.Gamma
  ( GammaT
  , runGammaT
  , withinEScope
  , withinTScope
  , getTLevel
  , lookupEVar
  ) where

import Pukeko.Prelude

import           Control.Lens

import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Scope
import           Pukeko.FrontEnd.Inferencer.UType

data GammaEnv s ev = GammaEnv
  { _tenv  :: EnvOf ev (UType s Void)
  , _level :: Int
  }
makeLenses ''GammaEnv

type GammaT s ev m = ReaderT (GammaEnv s ev) (SupplyT Id.TVar m)

runGammaT :: Monad m => GammaT s Void m a -> [Id.TVar] -> m a
runGammaT m vs = evalSupplyT (runReaderT m (GammaEnv (Const ()) 0)) vs

withinEScope ::
  forall i m s ev a. (HasEnvLevel i, HasEnv ev) =>
  EnvLevelOf i (UType s Void) -> GammaT s (EScope i ev) m a -> GammaT s ev m a
withinEScope ts = withReaderT (tenv %~ extendEnv @i @ev ts)

withinTScope :: (Monad m) => GammaT s ev m a -> GammaT s ev m a
withinTScope = local (level +~ 1)

getTLevel :: (Monad m) => GammaT s ev m Int
getTLevel = view level

lookupEVar :: (Monad m, HasEnv ev) => ev -> GammaT s ev m (UType s Void)
lookupEVar = views tenv . lookupEnv
