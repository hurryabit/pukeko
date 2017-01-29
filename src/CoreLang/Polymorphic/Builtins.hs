module CoreLang.Polymorphic.Builtins
  ( constructors
  , primitives
  , everything
  )
  where

import CoreLang.Language.Syntax (Identifier)
import CoreLang.Language.Type

alpha, beta, gamma :: Type
alpha = var "A"
beta  = var "B"
gamma = var "C"

constructors, primitives, everything :: [(Identifier, Type)]
constructors =
  [ ("false"  , bool)
  , ("true"   , bool)
  , ("mk_pair", alpha ~> beta ~> pair alpha beta)
  , ("nil"    , list alpha)
  , ("cons"   , alpha ~> list alpha ~> list alpha)
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
  , ("if"       , bool ~> alpha ~> alpha ~> alpha)
  , ("case_pair", pair alpha beta ~> (alpha ~> beta ~> gamma) ~> gamma)
  , ("case_list", list alpha ~> beta ~> (alpha ~> list alpha ~> beta) ~> beta)
  , ("print"    , int ~> alpha ~> alpha)
  , ("abort"    , alpha)
  ]
everything = constructors ++ primitives
