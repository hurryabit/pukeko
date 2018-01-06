{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
module Pukeko.Language.AST.Stage where

import GHC.TypeLits

import Pukeko.Language.Type (NoType, Type)

data Parser
data Renamer
data TypeResolver
data KindChecker
data FunResolver
data TypeChecker (t :: * -> *)
data PatternMatcher
data DeadCode
data TypeEraser
data LambdaLifter
data CoreCompiler

type family StageId st where
  StageId Parser          =   0
  StageId Renamer         = 100
  StageId TypeResolver    = 200
  StageId FunResolver     = 250
  StageId KindChecker     = 300
  StageId (TypeChecker t) = 400
  StageId PatternMatcher  = 500
  StageId DeadCode        = 600
  StageId TypeEraser      = 650
  StageId LambdaLifter    = 700
  StageId CoreCompiler    = 999

type family StageType st where
  StageType Parser          = NoType
  StageType Renamer         = NoType
  StageType TypeResolver    = NoType
  StageType FunResolver     = NoType
  StageType KindChecker     = NoType
  StageType (TypeChecker t) = t
  StageType PatternMatcher  = Type
  StageType DeadCode        = Type
  StageType TypeEraser      = NoType
  StageType LambdaLifter    = NoType
  StageType CoreCompiler    = NoType

type HasELam st = StageId st <=? 650
type HasEMat st = StageId st <=? 400
type HasETyp st = (400 <=? StageId st) && (StageId st <=? 600)

type HasTLTyp st = StageId st <=? 250
type HasTLVal st = StageId st <=? 275  -- NOTE: This odd number is a hack for
                                        -- the pretty printer
type HasTLLet st = StageId st <=? 350
type HasTLDef st = (400 <=? StageId st) && (StageId st <=? 650)
type HasTLSup st = 700 <=? StageId st

type HasMICons st = 200 <=? StageId st
type HasMIFuns st = 250 <=? StageId st

type SameTopNodes st1 st2 =
  ( HasTLTyp st1 ~ HasTLTyp st2
  , HasTLVal st1 ~ HasTLVal st2
  , HasTLLet st1 ~ HasTLLet st2
  , HasTLDef st1 ~ HasTLDef st2
  , HasTLSup st1 ~ HasTLSup st2
  )
type SameNodes st1 st2 =
  ( HasELam st1 ~ HasELam st2
  , HasEMat st1 ~ HasEMat st2
  , HasETyp st1 ~ HasETyp st2
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
