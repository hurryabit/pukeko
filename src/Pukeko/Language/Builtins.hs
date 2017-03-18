module Pukeko.Language.Builtins
  ( everything
  , types
  )
  where

import Pukeko.Language.ADT
import Pukeko.Language.Syntax (Ident (..))
import Pukeko.Language.Type

everything :: [(Ident, Type Closed)]
everything = map (\(i, t) -> (MkIdent i, t)) primitives

primitives :: [(String, Type Closed)]
primitives =
  [ ("neg", int  ~> int)
  , ("prefix_add", int  ~> int  ~> int )
  , ("prefix_sub", int  ~> int  ~> int )
  , ("prefix_mul", int  ~> int  ~> int )
  , ("prefix_div", int  ~> int  ~> int )
  , ("prefix_mod", int  ~> int  ~> int )
  , ("prefix_lt" , int  ~> int  ~> bool)
  , ("prefix_le" , int  ~> int  ~> bool)
  , ("prefix_eq" , int  ~> int  ~> bool)
  , ("prefix_ne" , int  ~> int  ~> bool)
  , ("prefix_ge" , int  ~> int  ~> bool)
  , ("prefix_gt" , int  ~> int  ~> bool)
  , ("return", alpha ~> io alpha)
  , ("print" , int ~> io unit)
  , ("prefix_bind", io alpha ~> (alpha ~> io beta) ~> io beta)
  , ("abort" , alpha)
  ]

types :: [ADT]
types =
  [ mkADT (MkIdent "Int") []            []
  , mkADT (MkIdent "IO")  [MkIdent "a"] []
  ]
