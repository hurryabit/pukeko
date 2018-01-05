{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module Data.Vector.Sized
  ( -- * Types
    Vector
  , Finite
    -- * Functions
  , (!)
  , (++)
  , withList
  , matchList
  , zip
  )
where

import           Prelude hiding ((++), zip)

import           Control.Lens.Indexed
import           Data.Finite
import           Data.Proxy
import qualified Data.Vector as V
import           GHC.TypeLits

newtype Vector (n :: Nat) a = MkVector (V.Vector a)
  deriving (Functor, Foldable, Traversable)

(!) :: Vector n a -> Finite n -> a
MkVector v ! i = v V.! toInt i

(++) :: Vector m a -> Vector n a -> Vector (m+n) a
MkVector v ++ MkVector w = MkVector (v V.++ w)

withList :: forall a r. [a] -> (forall n. KnownNat n => Vector n a -> r) -> r
withList xs k =
  case someNatVal (fromIntegral (V.length v)) of
    Just (SomeNat (Proxy :: Proxy n)) -> k (MkVector v :: Vector n a)
    Nothing -> error "You gave me a list of negative length. Well done!"
  where v = V.fromList xs

matchList :: Vector n a -> [b] -> Maybe (Vector n b)
matchList (MkVector v0) xs
  | V.length v0 == V.length v = Just (MkVector v)
  | otherwise                 = Nothing
  where v = V.fromList xs

zip :: Vector n a -> Vector n b -> Vector n (a, b)
zip (MkVector xs) (MkVector ys) = MkVector (V.zip xs ys)

instance FunctorWithIndex (Finite n) (Vector n)
instance FoldableWithIndex (Finite n) (Vector n)
instance TraversableWithIndex (Finite n) (Vector n) where
  itraverse f (MkVector v) =
    -- TODO: Make this more efficient.
    MkVector <$> traverse (uncurry f) (V.imap (\i x -> (unsafeFromInt i, x)) v)
