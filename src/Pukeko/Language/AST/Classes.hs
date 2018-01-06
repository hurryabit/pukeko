module Pukeko.Language.AST.Classes where

import Control.Lens (Lens')

class HasLhs a where
  type Lhs a
  lhs :: Lens' a (Lhs a)

class HasRhs a where
  type Rhs a
  rhs :: Lens' a (Rhs a)
