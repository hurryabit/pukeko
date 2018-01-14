{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Supply where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.RWS
import Data.Bifunctor (first, second)
import Data.Function
import Data.Tuple

class Monad m => MonadSupply s m | m -> s where
  fresh :: m s
  unfresh :: s -> m ()
  reset :: m ()
  resetWith :: [s] -> m ()


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
  unfresh x = SupplyT (modify (first (x:)))
  reset = SupplyT (modify (\(_, ys) -> (ys, ys)))
  resetWith xs = SupplyT (modify (first (const xs)))

instance (MonadSupply s m) => MonadSupply s (ReaderT r m) where
  fresh = lift fresh
  unfresh = lift . unfresh
  reset = lift reset
  resetWith = lift . resetWith

instance (MonadSupply t m, Monoid w) => MonadSupply t (RWST r w s m) where
  fresh = lift fresh
  unfresh = lift . unfresh
  reset = lift reset
  resetWith = lift . resetWith

instance MonadState s m => MonadState s (SupplyT t m) where
  get = lift get
  put = lift . put
  state = lift . state
