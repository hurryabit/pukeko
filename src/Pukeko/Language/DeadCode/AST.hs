{-# LANGUAGE DataKinds #-}
module Pukeko.Language.DeadCode.AST
  ( Module
  , TopLevel
  , Defn
  , Expr
  , Case
  ) where

import           Pukeko.Language.AST.Std

data DEADCODE

instance Stage DEADCODE where
  type StageId DEADCODE = 600

type Module = StdModule DEADCODE
type TopLevel = StdTopLevel DEADCODE
type Defn = StdDefn DEADCODE
type Expr = StdExpr DEADCODE
type Case = StdCase DEADCODE
