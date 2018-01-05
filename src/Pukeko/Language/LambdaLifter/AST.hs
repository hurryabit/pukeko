{-# LANGUAGE DataKinds #-}
module Pukeko.Language.LambdaLifter.AST
  ( ModuleInfo
  , Module
  , TopLevel
  , Defn
  , Expr
  , Case
  )
where

import           Pukeko.Language.AST.Std

data LAMBDALIFTER

instance Stage LAMBDALIFTER where
  type StageId LAMBDALIFTER = 700

type ModuleInfo = GenModuleInfo 'True
type Module = StdModule LAMBDALIFTER
type TopLevel = StdTopLevel LAMBDALIFTER
type Defn = StdDefn LAMBDALIFTER
type Expr = StdExpr LAMBDALIFTER
type Case = StdCase LAMBDALIFTER
