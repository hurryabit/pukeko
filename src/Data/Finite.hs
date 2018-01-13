{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module Data.Finite
  ( Finite
  , absurd0
  , unsafeFromInt
  , toInt
  , shift
  , weaken
  )
where

import Data.Proxy
import GHC.TypeLits

newtype Finite (n :: Nat) = Finite Int
  deriving (Eq, Ord, Show)

absurd0 :: Finite 0 -> a
absurd0 (Finite _) = error "absurd0"

unsafeFromInt :: Int -> Finite n
unsafeFromInt = Finite

toInt :: Finite n -> Int
toInt (Finite i) = i

shift :: forall m n. KnownNat m => Finite n -> Finite (m+n)
shift (Finite i) = Finite (fromInteger (natVal (Proxy :: Proxy m)) + i)

weaken :: Finite m -> Finite (m+n)
weaken (Finite i) = Finite i
