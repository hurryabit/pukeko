{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
module Pukeko.Pretty
  ( Doc
  , Pretty (..)
  , PrettyPrec (..)
  , render
  , annotateId

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
import           Text.PrettyPrint.Annotated.HughesPJ hiding
                 (Doc, (<>), (<+>), render, hsep, vcat)

infixr 6 <+>, <:~>

(<+>), (<:~>) :: Doc -> Doc -> Doc
(<+>) = (PP.<+>)
x <:~> y = x <> ":" <+> y
{-# INLINE (<+>) #-}
{-# INLINE (<:~>) #-}

data SrcAnn = NameId Int

type Doc = PP.Doc SrcAnn

class Pretty a where
  pretty :: a -> Doc
  default pretty :: PrettyPrec a => a -> Doc
  pretty = prettyPrec 0

class Pretty a => PrettyPrec a where
  prettyPrec :: Int -> a -> Doc

render :: Bool -> Doc -> String
render = \case
  False -> PP.render
  True  -> renderDecorated start end
  where
    start = const ""
    end (NameId i) = PP.render (braces (pretty i))

annotateId :: Int -> Doc -> Doc
annotateId = annotate . NameId

hsep :: (Foldable t) => t Doc -> Doc
hsep = PP.hsep . toList

hsepMap :: (Foldable t) => (a -> Doc) -> t a -> Doc
hsepMap f = PP.hsep . map f . toList

vcat :: (Foldable t) => t Doc -> Doc
vcat = PP.vcat . toList

vcatMap :: (Foldable t) => (a -> Doc) -> t a -> Doc
vcatMap f = PP.vcat . map f . toList

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
