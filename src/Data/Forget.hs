-- | A type whose contents do not influence derive 'Eq' and 'Ord' instances.
module Data.Forget
  ( Forget (Forget)
  , remember
  )
where

import Control.Lens
import Data.Bifunctor

newtype Forget a = Forget{_remember :: a}

remember :: Iso (Forget a) (Forget b) a b
remember = iso _remember Forget

instance Eq (Forget a) where
  _ == _ = True

instance Ord (Forget a) where
  _ `compare` _ = EQ

instance Show a => Show (Forget a) where
  showsPrec lvl (Forget x) = showsPrec lvl x

instance Read a => Read (Forget a) where
  readsPrec lvl = map (first Forget) . readsPrec lvl
