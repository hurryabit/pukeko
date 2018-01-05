{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Pukeko.Language.AST.Stage where

import GHC.TypeLits

data Parser
data Renamer
data TypeResolver
data KindChecker
data TypeChecker
data PatternMatcher
data DeadCode
data LambdaLifter
data CoreCompiler

type family StageId st where
  StageId Parser         =   0
  StageId Renamer        = 100
  StageId TypeResolver   = 200
  StageId KindChecker    = 300
  StageId TypeChecker    = 400
  StageId PatternMatcher = 500
  StageId DeadCode       = 600
  StageId LambdaLifter   = 700
  StageId CoreCompiler   = 999

type HasLam st = StageId st <=? 600
type HasMat st = StageId st <=? 400

type HasTypDef st = StageId st <=? 200
type HasVal    st = StageId st <=? 300
type HasTopLet st = StageId st <=? 300
type HasDef    st = (400 <=? StageId st) && (StageId st <=? 600)
type HasSupCom st = 700 <=? StageId st

type HasCons st = 200 <=? StageId st

type SameTopNodes st1 st2 =
  ( HasTypDef st1 ~ HasTypDef st2
  , HasVal    st1 ~ HasVal    st2
  , HasTopLet st1 ~ HasTopLet st2
  , HasDef    st1 ~ HasDef    st2
  , HasSupCom st1 ~ HasSupCom st2
  )
type SameNodes st1 st2 = (HasLam st1 ~ HasLam st2, HasMat st1 ~ HasMat st2)

type SameModuleInfo st1 st2 = HasCons st1 ~ HasCons st2

type family (&&) (x :: Bool) (y :: Bool) where
  'True  && 'True  = 'True
  'True  && 'False = 'False
  'False && 'True  = 'False
  'False && 'False = 'False
