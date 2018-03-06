{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
module Pukeko.Pretty
  ( Doc
  , Pretty (..)
  , PrettyPrec (..)
  , render

  , (<>)
  , (<+>)
  , (<:~>)
  , hsep
  , hsepMap
  , vcat
  , vcatMap
  , maybeParens

  , ($$)
  , ($+$)
  , braces
  , doubleQuotes
  , hang
  , isEmpty
  , nest
  , parens
  , punctuate
  , quotes
  , sep
  )
  where

import Prelude

import           Data.Foldable
import           Data.Tagged
import           Data.Void
import           Data.Semigroup (Semigroup (..))
import           Text.Megaparsec.Pos (SourcePos, sourcePosPretty)
import qualified Text.PrettyPrint.Annotated as PP
import           Text.PrettyPrint.Annotated hiding ((<>), (<+>), hsep, vcat)

infixr 6 <+>, <:~>

(<+>), (<:~>) :: Doc ann -> Doc ann -> Doc ann
(<+>) = (PP.<+>)
x <:~> y = x <> ":" <+> y
{-# INLINE (<+>) #-}
{-# INLINE (<:~>) #-}

class Pretty a where
  pretty :: a -> Doc ann
  default pretty :: PrettyPrec a => a -> Doc ann
  pretty = prettyPrec 0

class Pretty a => PrettyPrec a where
  prettyPrec :: Int -> a -> Doc ann

hsep :: (Foldable t) => t (Doc ann) -> Doc ann
hsep = PP.hsep . toList

hsepMap :: (Foldable t) => (a -> Doc ann) -> t a -> Doc ann
hsepMap f = PP.hsep . map f . toList

vcat :: (Foldable t) => t (Doc ann) -> Doc ann
vcat = PP.vcat . toList

vcatMap :: (Foldable t) => (a -> Doc ann) -> t a -> Doc ann
vcatMap f = PP.vcat . map f . toList

maybeParens :: Bool -> Doc ann -> Doc ann
maybeParens = \case
  False -> id
  True  -> parens

instance Pretty String where
  pretty = PP.text

instance Pretty Int where
  pretty = PP.int

instance Pretty a => Pretty (Tagged tag a) where
  pretty = pretty . untag

instance Pretty Void where
  pretty = absurd

instance Pretty SourcePos where
  pretty = pretty . sourcePosPretty
