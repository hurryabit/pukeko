{-# LANGUAGE DataKinds #-}
module Pukeko.Language.KindChecker.AST
  ( ModuleInfo
  , Module
  , TopLevel
  , Defn
  , Expr
  , Altn
  , Patn
  )
where

import           Pukeko.Language.AST.Std

data KINDCHECKER

instance Stage KINDCHECKER where
  type StageId KINDCHECKER = 300

type ModuleInfo = GenModuleInfo 'True
type TopLevel = StdTopLevel KINDCHECKER
type Module = StdModule KINDCHECKER
type Defn = StdDefn KINDCHECKER
type Expr = StdExpr KINDCHECKER
type Altn = StdAltn KINDCHECKER
type Patn = StdPatn KINDCHECKER
