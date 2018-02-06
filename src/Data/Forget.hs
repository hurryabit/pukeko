-- | A type whose contents do not influence derive 'Eq' and 'Ord' instances.
module Data.Forget
  ( Forget (Forget)
  , remember
  )
where

import Prelude
import Control.Lens (Iso, iso)
import Data.Aeson.TH

newtype Forget a = Forget{_remember :: a}

remember :: Iso (Forget a) (Forget b) a b
remember = iso _remember Forget

instance Eq (Forget a) where
  _ == _ = True

instance Ord (Forget a) where
  _ `compare` _ = EQ

instance Show a => Show (Forget a) where
  showsPrec lvl (Forget x) = showsPrec lvl x

deriveJSON defaultOptions ''Forget
