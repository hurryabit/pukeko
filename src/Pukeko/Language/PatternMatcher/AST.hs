{-# LANGUAGE DataKinds #-}
module Pukeko.Language.PatternMatcher.AST
  ( ModuleInfo
  , Module
  , TopLevel
  , Defn
  , Expr
  , Case
  ) where

import           Pukeko.Language.AST.Std

data PATTERNMATCHER

instance Stage PATTERNMATCHER where
  type StageId PATTERNMATCHER = 500

type ModuleInfo = StdModuleInfo PATTERNMATCHER
type Module = StdModule PATTERNMATCHER
type TopLevel = StdTopLevel PATTERNMATCHER
type Defn = StdDefn PATTERNMATCHER
type Expr = StdExpr PATTERNMATCHER
type Case = StdCase PATTERNMATCHER
