module Pukeko.AST.Language
  ( Surface
  , PreTyped
  , Typed
  , Unnested
  , Unclassy
  , SystemF
  , SuperCore

  , TypeOf
  , DictOf
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

import Data.Kind (Constraint)
import GHC.TypeLits (Nat, type (<=?), type (-))

import Pukeko.AST.Type (NoType, Type)
import Pukeko.AST.Dict (Dict)

data Surface
data PreTyped (t :: *) (d :: *)
data Typed
data Unnested
data Unclassy
type SystemF = Unclassy
data SuperCore

type family LangId lg :: Nat where
  LangId Surface        = 100
  LangId (PreTyped t d) = 399
  LangId Typed          = 400
  LangId Unnested       = 500
  LangId Unclassy       = 600
  LangId SuperCore      = 700

type family TypeOf lg :: * where
  TypeOf Surface        = NoType
  TypeOf (PreTyped t d) = t
  TypeOf Typed          = Type
  TypeOf Unnested       = Type
  TypeOf Unclassy       = Type
  TypeOf SuperCore      = Type

type family DictOf lg :: * where
  DictOf Surface        = Void
  DictOf (PreTyped t d) = d
  DictOf Typed          = Dict
  DictOf Unnested       = Dict
  DictOf Unclassy       = Dict
  DictOf SuperCore      = Dict

type IsPreTyped lg = LangId (PreTyped Type Dict) <=? LangId lg
type IsTyped    lg = (TypeOf lg ~ Type, DictOf lg ~ Dict)
type IsNested   lg = LangId lg <? LangId Unnested
type IsClassy   lg = LangId lg <? LangId Unclassy
type IsLambda   lg = LangId lg <? LangId SuperCore

type HasTmAbs lg = (() :: Constraint)
type HasTyApp lg =  IsPreTyped lg ~ True
type HasTyAbs lg =  IsPreTyped lg ~ True
type HasCxApp lg = (IsPreTyped lg ~ True, IsClassy lg ~ True)
type HasCxAbs lg = (IsPreTyped lg ~ True, IsClassy lg ~ True)

type (<?) m n = m <=? n-1
