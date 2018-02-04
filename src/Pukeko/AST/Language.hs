{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Pukeko.AST.Language where

import Pukeko.Prelude

import GHC.TypeLits (type (<=?), type (-))

import Pukeko.AST.Type (NoType, Type)

data Surface
data PreTyped (t :: * -> *)
data Typed
data Unnested
data Unclassy
type SystemF = Unclassy
data SuperCore

type family LangId lg where
  LangId Surface      = 100
  LangId (PreTyped t) = 399
  LangId Typed        = 400
  LangId Unnested     = 500
  LangId Unclassy     = 600
  LangId SuperCore    = 700

type family TypeOf lg where
  TypeOf Surface      = NoType
  TypeOf (PreTyped t) = t
  TypeOf Typed        = Type
  TypeOf Unnested     = Type
  TypeOf Unclassy     = Type
  TypeOf SuperCore    = Type

type IsLang lg = Traversable (TypeOf lg)

type IsPreTyped lg = LangId (PreTyped Type) <=? LangId lg
type IsTyped    lg = ((LangId Typed <=? LangId lg) ~ True, TypeOf lg ~ Type)
type IsNested   lg = LangId lg <? LangId Unnested
type IsClassy   lg = LangId lg <? LangId Unclassy
type IsLambda   lg = LangId lg <? LangId SuperCore

type SameNodes lg1 lg2 =
  ( IsPreTyped lg1 ~ IsPreTyped lg2
  , IsNested   lg1 ~ IsNested   lg2
  , IsLambda   lg1 ~ IsLambda   lg2
  )

type (<?) m n = m <=? n-1
