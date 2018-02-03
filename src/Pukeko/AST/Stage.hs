module Pukeko.AST.Stage where

import Pukeko.Prelude

import GHC.TypeLits (type (<=?))

import Pukeko.AST.Type (NoType, Type)

data Parser
data Renamer
data KindChecker
data Inferencer (t :: * -> *)
data PatternMatcher
data ClassEliminator
type FrontEnd = ClassEliminator
data LambdaLifter
type BackEnd = LambdaLifter

type family StageId st where
  StageId Renamer         = 100
  StageId KindChecker     = 300
  StageId (Inferencer t)  = 400
  StageId PatternMatcher  = 500
  StageId ClassEliminator = 600
  StageId LambdaLifter    = 700

type family StType st where
  StType Renamer         = NoType
  StType KindChecker     = NoType
  StType (Inferencer t)  = t
  StType PatternMatcher  = Type
  StType ClassEliminator = Type
  StType LambdaLifter    = Type

type IsStage st = Traversable (StType st)

type HasLambda st = StageId st <=? 650
type HasNested st = StageId st <=? 450
type HasTypes  st = 400 <=? StageId st
type HasClasses st = StageId st <=? 550

type SameDecls st1 st2 =
  ( Untyped   st1 ~ Untyped   st2
  , HasLambda st1 ~ HasLambda st2
  )

type SameNodes st1 st2 =
  ( HasLambda st1 ~ HasLambda st2
  , HasNested st1 ~ HasNested st2
  , HasTypes  st1 ~ HasTypes  st2
  )

type SameTypes st1 st2 = (StType st1 ~ StType st2)

-- NOTE: We cannot express that @Untyped@ shall be the negation of @Typed@ in
-- the type system, so we need to ensure this by hand.
type Typed st = (StType st ~ Type)
type Untyped st = (StageId st <=? 300)

type family (&&) (x :: Bool) (y :: Bool) where
  'True  && 'True  = 'True
  'True  && 'False = 'False
  'False && 'True  = 'False
  'False && 'False = 'False
