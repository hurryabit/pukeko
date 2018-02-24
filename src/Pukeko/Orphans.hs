{-# OPTIONS_GHC -Wno-orphans #-}
module Pukeko.Orphans () where

import Bound
import Bound.Name
import Data.Aeson
import Data.Aeson.TH
import Data.Void
import Text.Megaparsec.Pos (Pos, SourcePos)

instance ToJSON Void where
  toJSON = absurd

-- from megaparsec
deriveToJSON defaultOptions ''Pos
deriveToJSON defaultOptions ''SourcePos

-- from bound
deriveToJSON1 defaultOptions ''Var
deriveToJSON1 defaultOptions ''Scope
deriveToJSON2 defaultOptions ''Name

instance ToJSON n => ToJSON1 (Name n) where
  liftToJSON nameToJSON nameToJSONList =
    liftToJSON2 toJSON toJSONList nameToJSON nameToJSONList

instance (ToJSON n, ToJSON b) => ToJSON (Name n b) where
  toJSON = liftToJSON toJSON toJSONList
