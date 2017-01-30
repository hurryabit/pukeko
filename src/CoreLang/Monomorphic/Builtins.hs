module CoreLang.Monomorphic.Builtins
  ( constructors
  , primitives
  , everything
  )
  where

import Control.Arrow (first)

import CoreLang.Language.Ident
import CoreLang.Language.Type

constructors, primitives, everything :: [(Ident, Type)]
constructors = map (first MkIdent)
  [ ("false"  , bool)
  , ("true"   , bool)
  , ("nil"    , list int)
  , ("cons"   , int ~> list int ~> list int)
  ]
primitives = map (first MkIdent)
  [ ("neg", int  ~> int         )
  , ("+"  , int  ~> int  ~> int )
  , ("-"  , int  ~> int  ~> int )
  , ("*"  , int  ~> int  ~> int )
  , ("/"  , int  ~> int  ~> int )
  , ("<"  , int  ~> int  ~> bool)
  , ("<=" , int  ~> int  ~> bool)
  , ("==" , int  ~> int  ~> bool)
  , ("!=" , int  ~> int  ~> bool)
  , (">=" , int  ~> int  ~> bool)
  , (">"  , int  ~> int  ~> bool)
  , ("not", bool ~> bool        )
  , ("&&" , bool ~> bool ~> bool)
  , ("||" , bool ~> bool ~> bool)
  , ("if"       , bool ~> int ~> int ~> int)
  , ("case_list", list int ~> int ~> (int ~> list int ~> int) ~> int)
  , ("print"    , int ~> int ~> int)
  , ("abort"    , int)
  ]
everything = constructors ++ primitives
