{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
module Pukeko.AST.Stage where

import Pukeko.Prelude

import Control.Lens

import Pukeko.AST.Type (NoType, Type)

data Parser
data Renamer
data TypeResolver
data KindChecker
data FunResolver
data Inferencer (t :: * -> *)
data PatternMatcher
type FrontEnd = PatternMatcher
data LambdaLifter
type BackEnd = LambdaLifter

type family StageId st where
  StageId Renamer         = 100
  StageId TypeResolver    = 200
  StageId FunResolver     = 250
  StageId KindChecker     = 300
  StageId (Inferencer t)  = 400
  StageId PatternMatcher  = 500
  StageId LambdaLifter    = 700

type family StageType st where
  StageType Renamer         = NoType
  StageType TypeResolver    = NoType
  StageType FunResolver     = NoType
  StageType KindChecker     = NoType
  StageType (Inferencer t)  = t
  StageType PatternMatcher  = Type
  StageType LambdaLifter    = Type

type IsStage st = TraversableWithIndex Pos (StageType st)

type HasLambda st = StageId st <=? 650
type HasNested st = StageId st <=? 450
type HasTypes  st = 400 <=? StageId st

type SameTopNodes st1 st2 =
  ( Untyped   st1 ~ Untyped   st2
  , HasLambda st1 ~ HasLambda st2
  )
type SameNodes st1 st2 =
  ( HasLambda st1 ~ HasLambda st2
  , HasNested st1 ~ HasNested st2
  , HasTypes  st1 ~ HasTypes  st2
  )

type SameTypes st1 st2 = (StageType st1 ~ StageType st2)

-- NOTE: We cannot express that @Untyped@ shall be the negation of @Typed@ in
-- the type system, so we need to ensure this by hand.
type Typed st = (StageType st ~ Type)
type Untyped st = (StageId st <=? 300)

type family (&&) (x :: Bool) (y :: Bool) where
  'True  && 'True  = 'True
  'True  && 'False = 'False
  'False && 'True  = 'False
  'False && 'False = 'False
