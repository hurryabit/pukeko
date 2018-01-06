module Pukeko.Language.AST.Classes where

import Control.Lens (Lens', Lens)

class HasLhs a where
  type Lhs a
  lhs :: Lens' a (Lhs a)

class HasRhs a where
  type Rhs a
  rhs :: Lens' a (Rhs a)

-- class HasLhs1 (t :: * -> *) where
--   type Lhs1 t :: * -> *
--   lhs1 :: Lens (t a) (t b) (Lhs1 t a) (Lhs1 t b)

class HasRhs1 (t :: * -> *) where
  type Rhs1 t :: * -> *
  rhs1 :: Lens (t a) (t b) (Rhs1 t a) (Rhs1 t b)
