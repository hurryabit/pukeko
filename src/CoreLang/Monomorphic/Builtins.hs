module CoreLang.Monomorphic.Builtins
  ( constructors
  , primitives
  , everything
  )
  where

import CoreLang.Language.Syntax (Identifier)
import CoreLang.Language.Type

constructors, primitives, everything :: [(Identifier, Type)]
constructors =
  [ ("false"  , bool)
  , ("true"   , bool)
  , ("nil"    , list int)
  , ("cons"   , int ~> list int ~> list int)
  ]
primitives = 
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
