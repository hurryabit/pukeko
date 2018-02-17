{-# OPTIONS_GHC -Wno-orphans #-}
module Pukeko.Orphans () where

import Data.Aeson
import Data.Aeson.TH
import Data.Void
import Text.Megaparsec.Pos (Pos, SourcePos)

instance ToJSON Void where
  toJSON = absurd

-- from megaparsec
deriveToJSON defaultOptions ''Pos
deriveToJSON defaultOptions ''SourcePos
