module Data.Map.Extended
  ( module Data.Map.Lazy

  , (!)
  , fromMultiList
  )
where

import Prelude  hiding (lookup)
import Data.Map.Lazy hiding (foldr, (!))
import Data.Maybe (fromMaybe)
import Data.CallStack

infixl 9 !

(!) :: (HasCallStack, Ord k) => Map k v -> k -> v
m ! k =
  fromMaybe (error "Map.!: given key is not an element in the map") (k `lookup` m)

fromMultiList :: Ord k => [(k, v)] -> Map k [v]
fromMultiList = foldr f empty
  where
    f (k, v) = alter (Just . maybe [v] (v:)) k
