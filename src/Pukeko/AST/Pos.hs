{-# LANGUAGE UndecidableInstances #-}
module Pukeko.AST.Pos
  ( SourcePos
  , noPos
  , HasPos (..)
  , Where (..)
  , here
  , here'
  , Lctd (..)
  , lctd
  ) where

import Prelude
import Control.Lens (Lens)
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Data.Aeson
import Data.Functor.Compose
import Text.Megaparsec.Pos (SourcePos, initialPos)

import Pukeko.Pretty

noPos :: SourcePos
noPos = initialPos ""

class HasPos a where
  getPos :: a -> SourcePos

instance HasPos SourcePos where
  getPos = id

instance HasPos a => HasPos (a, b) where
  getPos = getPos . fst

class Where f where
  where_ :: f SourcePos
  here_ :: SourcePos -> f a -> f a

instance (Member (Reader SourcePos) effs) => Where (Eff effs) where
  where_ = ask
  here_ pos = local (const pos)

instance (Applicative f) => Where (Compose ((->) SourcePos) f) where
  where_ = Compose pure
  here_ pos (Compose f) = Compose (const (f pos))

here :: (Where f, HasPos a) => a -> f b -> f b
here = here_ . getPos

here' :: (Where f, HasPos a) => (a -> f b) -> a -> f b
here' f x = here x (f x)

data Lctd a = Lctd{pos :: SourcePos, unlctd :: a}
  deriving (Show, Foldable, Functor, Traversable)

lctd :: Lens (Lctd a) (Lctd b) a b
lctd f (Lctd pos thng) = Lctd pos <$> f thng

instance HasPos (Lctd a) where
  getPos = pos

instance ToJSON a => ToJSON (Lctd a) where
  toJSON = toJSON . unlctd

instance Pretty a => Pretty (Lctd a) where
  pretty = pretty . unlctd

instance PrettyPrec a => PrettyPrec (Lctd a) where
  prettyPrec prec = prettyPrec prec . unlctd
