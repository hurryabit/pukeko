{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
module Control.Monad.Freer.Supply
  ( Supply (..)
  , fresh
  , reset
  , resetWith
  , evalSupply
  ) where

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Natural (type (~>))
import Data.Function ((.))

data Supply s a where
  Fresh :: Supply s s
  Reset :: Supply s ()
  ResetWith :: [s] -> Supply s ()

fresh :: (Member (Supply s) effs) => Eff effs s
fresh = send Fresh

reset :: forall s effs. (Member (Supply s) effs) => Eff effs ()
reset = send (Reset @s)

resetWith :: (Member (Supply s) effs) => [s] -> Eff effs ()
resetWith = send . ResetWith

evalSupply :: forall s effs a. [s] -> Eff (Supply s : effs) a -> Eff effs a
evalSupply xs0 = evalState xs0 . reinterpret f
  where
    f :: Supply s ~> Eff (State [s] : effs)
    f = \case
      Fresh -> get >>= \(x:xs) -> put xs >> return x
      Reset -> put xs0
      ResetWith xs1 -> put xs1
