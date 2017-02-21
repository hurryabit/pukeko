module Pukeko.Language.Builtins
  ( constructors
  , primitives
  , everything
  )
  where

import Pukeko.Language.Syntax (Ident (..))
import Pukeko.Language.Type

alpha, beta :: Type
alpha = var "A"
beta  = var "B"

constructors, primitives :: [(String, Type)]
constructors =
  [ ("false"  , bool)
  , ("true"   , bool)
  , ("nil"    , list alpha)
  , ("cons"   , alpha ~> list alpha ~> list alpha)
  , ("mk_pair", alpha ~> beta ~> pair alpha beta)
  ]
primitives =
  [ ("neg", int  ~> int         )
  , ("add", int  ~> int  ~> int )
  , ("sub", int  ~> int  ~> int )
  , ("mul", int  ~> int  ~> int )
  , ("div", int  ~> int  ~> int )
  , ("mod", int  ~> int  ~> int )
  , ("lt" , int  ~> int  ~> bool)
  , ("le" , int  ~> int  ~> bool)
  , ("eq" , int  ~> int  ~> bool)
  , ("ne" , int  ~> int  ~> bool)
  , ("ge" , int  ~> int  ~> bool)
  , ("gt" , int  ~> int  ~> bool)
  , ("not", bool ~> bool)
  , ("and", bool ~> bool ~> bool)
  , ("or" , bool ~> bool ~> bool)
  , ("if" , bool ~> alpha ~> alpha ~> alpha)
  , ("is_nil", list alpha ~> bool )
  , ("hd"    , list alpha ~> alpha)
  , ("tl"    , list alpha ~> list alpha)
  , ("fst"   , pair alpha beta ~> alpha)
  , ("snd"   , pair alpha beta ~> beta )
  , ("print" , int ~> alpha ~> alpha)
  , ("abort" , alpha)
  ]

everything :: [(Ident, Type)]
everything = map (\(i, t) -> (MkIdent i, t)) (constructors ++ primitives)
