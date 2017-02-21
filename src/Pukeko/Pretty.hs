module Pukeko.Pretty
  ( (<+>)
  , pretty
  , prettyPrint
  , perror
  , pthrow
  , module Data.Monoid
  , module Text.PrettyPrint.HughesPJClass
  )
  where

import Control.Monad.Except
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

perror :: Doc -> a
perror = error . render

pthrow :: MonadError String m => Doc -> m a
pthrow = throwError . render
