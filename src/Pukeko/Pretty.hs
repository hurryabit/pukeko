module Pukeko.Pretty
  ( (<+>)
  , pretty
  , prettyPrint
  , module Data.Monoid
  , module Text.PrettyPrint.HughesPJClass
  )
  where

import Data.Monoid
import Text.PrettyPrint.HughesPJClass hiding ((<>), (<+>))

import qualified Text.PrettyPrint.HughesPJClass as HughesPJClass ((<+>))

infixr 6 <+>

(<+>) :: Doc -> Doc -> Doc
(<+>) = (HughesPJClass.<+>)
{-# INLINE (<+>) #-}

pretty :: Pretty a => a -> Doc
pretty = pPrint

prettyPrint :: Pretty a => a -> IO ()
prettyPrint = print . pPrint
