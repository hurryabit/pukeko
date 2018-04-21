module Data.List.NE
  ( module Data.List.NonEmpty

  , (++)
  , singleton
  , zipWithM_
  , unzip3
  , toVector
  ) where

import Prelude hiding (unzip3, zipWith, (++))

import           Data.Foldable (sequence_)
import qualified Data.List as L
import           Data.List.NonEmpty
import qualified Data.Vector as V

(++) :: NonEmpty a -> NonEmpty a -> NonEmpty a
(x:|xs) ++ ys = x :| (xs L.++ toList ys)

singleton :: a -> NonEmpty a
singleton = (:| [])

zipWithM_ :: Monad m => (a -> b -> m ()) -> NonEmpty a -> NonEmpty b -> m ()
zipWithM_ f xs ys = sequence_ (zipWith f xs ys)

unzip3 :: NonEmpty (a, b, c) -> (NonEmpty a, NonEmpty b, NonEmpty c)
unzip3 ((x, y, z) :| xyzs) = (x :| xs, y :| ys, z :| zs)
  where (xs, ys, zs) = L.unzip3 xyzs

toVector :: NonEmpty a -> V.Vector a
toVector = V.fromList . toList
