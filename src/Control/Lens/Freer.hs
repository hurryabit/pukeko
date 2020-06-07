module Control.Lens.Freer
  ( use
  , uses
  , view
  , views
  , modifying
  , locally
  ) where

import Control.Lens hiding (locally, modifying, use, uses, view, views)
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State
import Data.Profunctor.Unsafe (( #. ))

use :: (Member (State s) effs) => Getting a s a -> Eff effs a
use l = gets (^.l)
{-# INLINE use #-}

uses :: (Member (State s) effs) => Getting a s a -> (a -> r) -> Eff effs r
uses l f = gets (\s -> f (s^.l))
{-# INLINE uses #-}

view :: (Member (Reader s) effs) => Getting a s a -> Eff effs a
view l = asks (getConst #. l Const)
{-# INLINE view #-}

views :: (Member (Reader s) effs) => LensLike' (Const r) s a -> (a -> r) -> Eff effs r
views l f = asks (getConst #. l (Const #. f))
{-# INLINE views #-}

modifying :: (Member (State s) effs) => ASetter s s a b -> (a -> b) -> Eff effs ()
modifying l f = modify (over l f)
{-# INLINE modifying #-}

locally :: Member (Reader s) effs =>
  ASetter s s a b -> (a -> b) -> Eff effs x -> Eff effs x
locally l f = local (l %~ f)
{-# INLINE locally #-}
