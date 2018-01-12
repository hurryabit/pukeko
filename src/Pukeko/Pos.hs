module Pukeko.Pos
  ( Pos
  , mkPos
  , noPos
  , HasPos (..)
  ) where

import Control.Lens (Lens')
import Text.Parsec  (SourcePos)

import Pukeko.Pretty

newtype Pos = Pos (Maybe SourcePos)

mkPos :: SourcePos -> Pos
mkPos = Pos . Just

noPos :: Pos
noPos = Pos Nothing

class HasPos a where
  pos :: Lens' a Pos

instance Pretty Pos where
  pPrintPrec _ _ (Pos p) = maybe "no position" (text . show) p
