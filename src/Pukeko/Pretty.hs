{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE PatternSynonyms #-}
module Pukeko.Pretty
  ( hsep
  , hsepMap
  , maybeParens

  , (<+>)
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
  , vcat
  )
  where

import Pukeko.Prelude

import qualified Text.PrettyPrint.Annotated as PP
import           Text.PrettyPrint.Annotated hiding ((<+>), hsep)

hsep :: (Foldable t) => t (Doc ann) -> Doc ann
hsep = PP.hsep . toList

hsepMap :: (Foldable t) => (a -> Doc ann) -> t a -> Doc ann
hsepMap f = PP.hsep . map f . toList

maybeParens :: Bool -> Doc ann -> Doc ann
maybeParens = \case
  False -> id
  True  -> parens
