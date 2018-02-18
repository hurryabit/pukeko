module Data.Map.Extended
  ( module Data.Map

  , (!)
  , fromMultiList
  )
where

import Prelude  hiding (lookup)
import Data.Map hiding (foldr, (!))
import Data.CallStack

infixl 9 !

(!) :: (HasCallStack, Ord k) => Map k v -> k -> v
m ! k = case k `lookup` m of
  Nothing -> error "Map.!: given key is not an element in the map"
  Just v  -> v

fromMultiList :: Ord k => [(k, v)] -> Map k [v]
fromMultiList = foldr f empty
  where
    f (k, v) = alter (Just . maybe [v] (v:)) k
