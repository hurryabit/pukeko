{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Supply where

import Control.Monad.State
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.Writer.Class
import Data.Bifunctor (second)

class Monad m => MonadSupply s m | m -> s where
  fresh :: m s
  reset :: m ()

newtype SupplyT s m a = SupplyT{unSupplyT :: StateT ([s], [s]) m a}
  deriving ( Functor, Applicative, Monad, MonadTrans
           , MonadError e
           , MonadReader r
           , MonadWriter w
           )

runSupplyT :: Monad m => SupplyT s m a -> [s] -> m (a, [s])
runSupplyT m xs = second fst <$> runStateT (unSupplyT m) (xs, xs)

evalSupplyT :: Monad m => SupplyT s m a -> [s] -> m a
evalSupplyT m xs = fst <$> runSupplyT m xs

instance Monad m => MonadSupply s (SupplyT s m) where
  fresh = SupplyT (state (\(x:xs, ys) -> (x, (xs, ys))))
  reset = SupplyT (state (\(_, ys) -> ((), (ys, ys))))

instance (MonadSupply s m) => MonadSupply s (ReaderT r m) where
  fresh = lift fresh
  reset = lift reset
