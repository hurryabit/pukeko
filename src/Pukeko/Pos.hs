module Pukeko.Pos where

import Control.Lens (Lens')
import Text.Parsec  (SourcePos)

type Pos = SourcePos

class HasPos a where
  pos :: Lens' a Pos
