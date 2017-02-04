{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Supply
  ( MonadSupply (..)
  , SupplyT (..)
  , runSupplyT
  , evalSupplyT
  , Supply
  , runSupply
  , evalSupply
  )
  where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

class Monad m => MonadSupply s m where
  fresh :: m s

newtype SupplyT s m a = SupplyT { unSupplyT :: StateT [s] m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

runSupplyT :: SupplyT s m a -> [s] -> m (a, [s])
runSupplyT m = runStateT (unSupplyT m)

evalSupplyT :: Monad m => SupplyT s m a -> [s] -> m a
evalSupplyT m = liftM fst . runSupplyT m

type Supply s = SupplyT s Identity

runSupply :: Supply s a -> [s] -> (a, [s])
runSupply m = runIdentity . runSupplyT m

evalSupply :: Supply s a -> [s] -> a
evalSupply m = runIdentity . evalSupplyT m

instance Monad m => MonadSupply s (SupplyT s m) where
  fresh = SupplyT $ state $ \(x:xs) -> (x,xs)

instance MonadSupply s m => MonadSupply s (ExceptT e m) where
  fresh = lift fresh

instance MonadSupply s m => MonadSupply s (ReaderT r m) where
  fresh = lift fresh

instance (Monoid w, MonadSupply s m) => MonadSupply s (WriterT w m) where
  fresh = lift fresh
