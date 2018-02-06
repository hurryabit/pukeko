{-# OPTIONS_GHC -Wno-orphans #-}
module Pukeko.Orphans () where

import Data.Aeson
import Data.Void

instance ToJSON Void where
  toJSON = absurd
