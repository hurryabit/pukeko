{-# OPTIONS_GHC -Wno-orphans #-}
module Pukeko.Pretty
  ( (<+>)
  , pretty
  , prettyPrint
  , hsep
  , module Data.Monoid
  , module Text.PrettyPrint.HughesPJClass
  )
  where

import Data.Foldable
import Data.Monoid
import Data.Void
import Text.PrettyPrint.HughesPJClass hiding ((<>), (<+>), hsep)

import qualified Text.PrettyPrint.HughesPJClass as PP

infixr 6 <+>

(<+>) :: Doc -> Doc -> Doc
(<+>) = (PP.<+>)
{-# INLINE (<+>) #-}

pretty :: Pretty a => a -> Doc
pretty = pPrint

hsep :: (Foldable t) => t Doc -> Doc
hsep = PP.hsep . toList

prettyPrint :: Pretty a => a -> IO ()
prettyPrint = print . pPrint

instance Pretty Void where
  pPrintPrec _ _ = absurd
