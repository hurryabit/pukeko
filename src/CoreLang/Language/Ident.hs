module CoreLang.Language.Ident
  ( Ident (..)
  )
  where

import CoreLang.Pretty

newtype Ident = MkIdent { unIdent :: String }
  deriving (Show, Eq, Ord)

instance Pretty Ident where
  pPrint (MkIdent x) = text x
