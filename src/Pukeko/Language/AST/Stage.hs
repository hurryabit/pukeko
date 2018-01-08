{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
module Pukeko.Language.AST.Stage where

import GHC.TypeLits

import Pukeko.Language.Type (NoType)

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
  StageId TypeEraser      = 450
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
  StageType TypeEraser      = NoType
  StageType PatternMatcher  = NoType
  StageType LambdaLifter    = NoType
  StageType CoreCompiler    = NoType

-- TODO: Giver proper names to these constraints.
type HasLambda st = StageId st <=? 650
type HasEMat   st = StageId st <=? 450
type HasETyp   st = (400 <=? StageId st) && (StageId st <=? 400)

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
  , HasEMat   st1 ~ HasEMat   st2
  , HasETyp   st1 ~ HasETyp   st2
  )

type SameTypes st1 st2 = (StageType st1 ~ StageType st2)

type SameModuleInfo st1 st2 =
  ( HasMICons st1 ~ HasMICons st2
  , HasMIFuns st1 ~ HasMIFuns st2
  )

type family (&&) (x :: Bool) (y :: Bool) where
  'True  && 'True  = 'True
  'True  && 'False = 'False
  'False && 'True  = 'False
  'False && 'False = 'False
