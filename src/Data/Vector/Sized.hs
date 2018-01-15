{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module Data.Vector.Sized
  ( -- * Types
    Vector
  , Finite
    -- * Functions
  , (!)
  , (++)
  , empty
  , singleton
  , plength
  , withList
  , withNonEmpty
  , matchList
  , matchNonEmpty
  , zip
  , zip3
  , zipWith
  , zipWith3
  , zipWithM
  , zipWithM_
  , zipWith3M_
  , unzip
  )
where

import           Prelude hiding ((++), zip, zip3, zipWith, zipWith3, unzip)

import           Control.Lens.At
import           Control.Lens.Indexed
import           Data.Finite
import qualified Data.List.NonEmpty as NE
import           Data.Proxy
import qualified Data.Vector as V
import           GHC.TypeLits

newtype Vector (n :: Nat) a = MkVector (V.Vector a)
  deriving (Eq, Functor, Foldable, Traversable)

(!) :: Vector n a -> Finite n -> a
MkVector v ! i = v V.! toInt i

(++) :: Vector m a -> Vector n a -> Vector (m+n) a
MkVector v ++ MkVector w = MkVector (v V.++ w)

empty :: Vector 0 a
empty = MkVector V.empty

singleton :: a -> Vector 1 a
singleton = MkVector . V.singleton

plength :: Vector n a -> Proxy n
plength _ = Proxy

withList :: forall a r. [a] -> (forall n. KnownNat n => Vector n a -> r) -> r
withList xs k =
  case someNatVal (fromIntegral (V.length v)) of
    Just (SomeNat (Proxy :: Proxy n)) -> k (MkVector v :: Vector n a)
    Nothing -> error "You gave me a list of negative length. Well done!"
  where v = V.fromList xs

withNonEmpty :: NE.NonEmpty a -> (forall n. KnownNat n => Vector n a -> r) -> r
withNonEmpty xs = withList (NE.toList xs)

matchList :: Vector n a -> [b] -> Maybe (Vector n b)
matchList (MkVector v0) xs
  | V.length v0 == V.length v = Just (MkVector v)
  | otherwise                 = Nothing
  where v = V.fromList xs

matchNonEmpty :: Vector n a -> NE.NonEmpty b -> Maybe (Vector n b)
matchNonEmpty v = matchList v . NE.toList

zip :: Vector n a -> Vector n b -> Vector n (a, b)
zip (MkVector xs) (MkVector ys) = MkVector (V.zip xs ys)

zip3 :: Vector n a -> Vector n b -> Vector n c -> Vector n (a, b, c)
zip3 (MkVector xs) (MkVector ys) (MkVector zs) = MkVector (V.zip3 xs ys zs)

zipWith :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
zipWith f (MkVector xs) (MkVector ys) = MkVector (V.zipWith f xs ys)

zipWith3 :: (a -> b -> c -> d) -> Vector n a -> Vector n b -> Vector n c -> Vector n d
zipWith3 f (MkVector xs) (MkVector ys) (MkVector zs) = MkVector (V.zipWith3 f xs ys zs)

zipWithM :: Monad m => (a -> b -> m c) -> Vector n a -> Vector n b -> m (Vector n c)
zipWithM f (MkVector xs) (MkVector ys) = MkVector <$> V.zipWithM f xs ys

zipWithM_ :: Monad m => (a -> b -> m c) -> Vector n a -> Vector n b -> m ()
zipWithM_ f (MkVector xs) (MkVector ys) = V.zipWithM_ f xs ys

zipWith3M_ ::
  Monad m => (a -> b -> c -> m ()) -> Vector n a -> Vector n b -> Vector n c -> m ()
zipWith3M_ f xs ys zs = sequence_ (zipWith3 f xs ys zs)

unzip :: Vector n (a, b) -> (Vector n a, Vector n b)
unzip (MkVector xys) = (MkVector xs, MkVector ys)
  where
    (xs, ys) = V.unzip xys

instance FunctorWithIndex (Finite n) (Vector n)
instance FoldableWithIndex (Finite n) (Vector n)
instance TraversableWithIndex (Finite n) (Vector n) where
  itraverse f (MkVector v) =
    -- TODO: Make this more efficient.
    MkVector <$> traverse (uncurry f) (V.imap (\i x -> (unsafeFromInt i, x)) v)

type instance Index   (Vector n a) = Finite n
type instance IxValue (Vector n a) = a

instance Ixed (Vector n a) where
  ix i f (MkVector v) = MkVector <$> ix (toInt i) f v

deriving instance Show a => Show (Vector n a)
