module Pukeko.Language.Ident
  ( Ident (..)
  , isConstructor
  , isVariable
  )
  where

import Data.Char (isLower, isUpper)
import Pukeko.Pretty

newtype Ident = MkIdent { unIdent :: String }
  deriving (Eq, Ord)

isConstructor, isVariable :: Ident -> Bool
isConstructor = isUpper . head . unIdent
isVariable    = isLower . head . unIdent

instance Show Ident where
  show (MkIdent s) = "Ident " ++ show s

instance Pretty Ident where
  pPrint (MkIdent x) = text x
