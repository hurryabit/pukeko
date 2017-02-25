module Pukeko.Language.Builtins
  ( constructors
  , primitives
  , everything
  )
  where

import Pukeko.Language.Syntax (Ident (..))
import Pukeko.Language.Type

alpha, beta :: Type
alpha = var "a"
beta  = var "b"

constructors, primitives :: [(String, Type)]
constructors =
  [ ("unit"   , unit)
  , ("false"  , bool)
  , ("true"   , bool)
  , ("nil"    , list alpha)
  , ("cons"   , alpha ~> list alpha ~> list alpha)
  , ("mk_pair", alpha ~> beta ~> pair alpha beta)
  ]
primitives =
  [ ("neg", int  ~> int         )
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
  , ("if" , bool ~> alpha ~> alpha ~> alpha)
  , ("is_nil", list alpha ~> bool )
  , ("hd"    , list alpha ~> alpha)
  , ("tl"    , list alpha ~> list alpha)
  , ("fst"   , pair alpha beta ~> alpha)
  , ("snd"   , pair alpha beta ~> beta )
  , ("return", alpha ~> io alpha)
  , ("print" , int ~> io unit)
  , ("prefix_bind", io alpha ~> (alpha ~> io beta) ~> io beta)
  , ("abort" , alpha)
  ]

everything :: [(Ident, Type)]
everything = map (\(i, t) -> (MkIdent i, t)) (constructors ++ primitives)
