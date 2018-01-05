{-# LANGUAGE DataKinds #-}
module Pukeko.Language.TypeChecker.AST
  ( ModuleInfo
  , Module
  , TopLevel
  , Defn
  , Expr
  , Altn
  , Patn
  ) where

import           Pukeko.Language.AST.Std

data TYPECHECKER

instance Stage TYPECHECKER where
  type StageId TYPECHECKER = 400

type ModuleInfo = GenModuleInfo 'True
type Module = StdModule TYPECHECKER
type TopLevel = StdTopLevel TYPECHECKER
type Defn = StdDefn TYPECHECKER
type Expr = StdExpr TYPECHECKER
type Altn = StdAltn TYPECHECKER
type Patn = StdPatn TYPECHECKER
