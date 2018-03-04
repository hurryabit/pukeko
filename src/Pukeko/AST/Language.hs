{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Pukeko.AST.Language
  ( Surface
  , PreTyped
  , Typed
  , Unnested
  , Unclassy
  , SystemF
  , SuperCore

  , TypeOf
  , IsPreTyped
  , IsTyped
  , IsNested
  , IsClassy
  , IsLambda

  , HasTmAbs
  , HasTyApp
  , HasTyAbs
  , HasCxApp
  , HasCxAbs
  )

where

import Pukeko.Prelude

import GHC.TypeLits (Nat, type (<=?), type (-))

import Pukeko.AST.Type (NoType, Type)

data Surface
data PreTyped (t :: *)
data Typed
data Unnested
data Unclassy
type SystemF = Unclassy
data SuperCore

type family LangId lg :: Nat where
  LangId Surface      = 100
  LangId (PreTyped t) = 399
  LangId Typed        = 400
  LangId Unnested     = 500
  LangId Unclassy     = 600
  LangId SuperCore    = 700

type family TypeOf lg :: * where
  TypeOf Surface      = NoType
  TypeOf (PreTyped t) = t
  TypeOf Typed        = Type
  TypeOf Unnested     = Type
  TypeOf Unclassy     = Type
  TypeOf SuperCore    = Type

type IsPreTyped lg = LangId (PreTyped Type) <=? LangId lg
type IsTyped    lg = ((LangId Typed <=? LangId lg) ~ True, TypeOf lg ~ Type)
type IsNested   lg = LangId lg <? LangId Unnested
type IsClassy   lg = LangId lg <? LangId Unclassy
type IsLambda   lg = LangId lg <? LangId SuperCore

type HasTmAbs lg =  IsLambda lg ~ True
type HasTyApp lg =                      IsPreTyped lg ~ True
type HasTyAbs lg =                      TypeOf lg ~ Type
type HasCxApp lg = (IsClassy lg ~ True, IsPreTyped lg ~ True)
type HasCxAbs lg = (IsClassy lg ~ True, TypeOf lg ~ Type)

type (<?) m n = m <=? n-1
