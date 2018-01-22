{-# LANGUAGE TypeApplications #-}
module Pukeko.FrontEnd.Inferencer.Gamma
  ( GammaT
  , runGammaT
  , withinEScope
  , withinTScope
  , withQVars
  , getTLevel
  , lookupEVar
  , lookupTVar
  ) where

import Pukeko.Prelude

import           Control.Lens
import           Data.Coerce (coerce)
import qualified Data.Map     as Map

import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Scope
import           Pukeko.AST.Type
import           Pukeko.FrontEnd.Inferencer.UType

data GammaEnv s ev = GammaEnv
  { _tenv  :: EnvOf ev (UType s Void)
  , _level :: Int
  , _qenv  :: Map Id.TVar (Set Id.Clss)
  }
makeLenses ''GammaEnv

type GammaT s ev m = HereT (ReaderT (GammaEnv s ev) (SupplyT Id.TVar m))

runGammaT :: Monad m => GammaT s Void m a -> [Id.TVar] -> m a
runGammaT m vs =
  evalSupplyT (runReaderT (runHereT m) (GammaEnv (Const ()) 0 mempty)) vs

withinEScope ::
  forall i m s tv ev a. (HasEnvLevel i, HasEnv ev) =>
  EnvLevelOf i (UType s tv) -> GammaT s (EScope i ev) m a -> GammaT s ev m a
withinEScope ts = mapHereT (withReaderT (tenv %~ extendEnv @i @ev (fmap coerce ts)))

withinTScope :: (Monad m) => GammaT s ev m a -> GammaT s ev m a
withinTScope = local (level +~ 1)

-- TODO: It's not entirely clear to me if not changing the level is the right
-- thing to do for future uses, particularly existential types.
withQVars :: (Monad m, Foldable t) => t QVar -> GammaT s ev m a -> GammaT s ev m a
withQVars qvs = local (qenv <>~ foldMap (\(MkQVar q v) -> Map.singleton v q) qvs)

getTLevel :: (Monad m) => GammaT s ev m Int
getTLevel = view level

lookupEVar :: (Monad m, HasEnv ev) => ev -> GammaT s ev m (UType s tv)
lookupEVar x = fmap absurd <$> views tenv (lookupEnv x)

lookupTVar :: (Monad m) => Id.TVar -> GammaT s ev m (Set Id.Clss)
lookupTVar x = views qenv (Map.findWithDefault (bugWith "lookupTVar" x) x)
