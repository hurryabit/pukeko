module CoreLang.Language.Ident
  ( Ident (..)
  )
  where

import CoreLang.Pretty

newtype Ident = MkIdent { unIdent :: String }
  deriving (Eq, Ord)

instance Show Ident where
  show (MkIdent s) = "Ident " ++ show s

instance Pretty Ident where
  pPrint (MkIdent x) = text x
