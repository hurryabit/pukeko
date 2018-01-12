{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
module Pukeko.Language.AST.Stage where

import Control.Lens
import GHC.TypeLits

import Pukeko.Pos
import Pukeko.Language.Type (NoType, Type)

data Parser
data Renamer
data TypeResolver
data KindChecker
data FunResolver
data Inferencer (t :: * -> *)
data PatternMatcher
data TypeEraser
data LambdaLifter
data CoreCompiler

type family StageId st where
  StageId Parser          =   0
  StageId Renamer         = 100
  StageId TypeResolver    = 200
  StageId FunResolver     = 250
  StageId KindChecker     = 300
  StageId (Inferencer t)  = 400
  StageId PatternMatcher  = 500
  StageId LambdaLifter    = 700
  StageId CoreCompiler    = 999

type family StageType st where
  StageType Parser          = NoType
  StageType Renamer         = NoType
  StageType TypeResolver    = NoType
  StageType FunResolver     = NoType
  StageType KindChecker     = NoType
  StageType (Inferencer t)  = t
  StageType PatternMatcher  = Type
  StageType LambdaLifter    = Type
  StageType CoreCompiler    = NoType

type IsStage st = TraversableWithIndex Pos (StageType st)

type HasLambda st = StageId st <=? 650
type HasNested st = StageId st <=? 450
type HasTypes  st = 400 <=? StageId st

type HasTLTyp st = StageId st <=? 250
type HasTLVal st = StageId st <=? 275  -- NOTE: This odd number is a hack for
                                       -- the pretty printer

type HasMICons st = 200 <=? StageId st
type HasMIFuns st = 250 <=? StageId st

type SameTopNodes st1 st2 =
  ( HasTLTyp  st1 ~ HasTLTyp  st2
  , HasTLVal  st1 ~ HasTLVal  st2
  , HasLambda st1 ~ HasLambda st2
  )
type SameNodes st1 st2 =
  ( HasLambda st1 ~ HasLambda st2
  , HasNested st1 ~ HasNested st2
  , HasTypes  st1 ~ HasTypes  st2
  )

type SameTypes st1 st2 = (StageType st1 ~ StageType st2)

type SameModuleInfo st1 st2 =
  ( HasMICons st1 ~ HasMICons st2
  , HasMIFuns st1 ~ HasMIFuns st2
  )

type Typed st =
  ( StageType st ~ Type
  , HasMICons st ~ 'True
  , HasMIFuns st ~ 'True
  )

type family (&&) (x :: Bool) (y :: Bool) where
  'True  && 'True  = 'True
  'True  && 'False = 'False
  'False && 'True  = 'False
  'False && 'False = 'False
