{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
module Control.Monad.Freer.Supply
  ( Supply
  , fresh
  , evalSupply
  ) where

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.State
import Data.Function ((.))

data Supply s a where
  Fresh :: Supply s s

fresh :: (Member (Supply s) effs) => Eff effs s
fresh = send Fresh

evalSupply :: forall s effs a. [s] -> Eff (Supply s : effs) a -> Eff effs a
evalSupply xs0 = evalState xs0 . reinterpret f
  where
    f :: Supply s ~> Eff (State [s] : effs)
    f Fresh = get >>= \(x:xs) -> put xs >> return x
