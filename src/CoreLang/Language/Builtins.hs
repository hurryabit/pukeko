module CoreLang.Language.Builtins
  ( constructors
  , primitives
  , everything
  )
  where

import CoreLang.Language.Syntax (Ident (..))
import CoreLang.Language.Type

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
  , ("+"  , int  ~> int  ~> int )
  , ("-"  , int  ~> int  ~> int )
  , ("*"  , int  ~> int  ~> int )
  , ("/"  , int  ~> int  ~> int )
  , ("%"  , int  ~> int  ~> int )
  , ("<"  , int  ~> int  ~> bool)
  , ("<=" , int  ~> int  ~> bool)
  , ("==" , int  ~> int  ~> bool)
  , ("!=" , int  ~> int  ~> bool)
  , (">=" , int  ~> int  ~> bool)
  , (">"  , int  ~> int  ~> bool)
  , ("not", bool ~> bool)
  , ("&&" , bool ~> bool ~> bool)
  , ("||" , bool ~> bool ~> bool)
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