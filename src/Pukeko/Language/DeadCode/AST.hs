{-# LANGUAGE DataKinds #-}
module Pukeko.Language.DeadCode.AST
  ( Module
  , PM.StdTopLevel (..)
  , TopLevel
  , Defn
  , Expr
  , Case
  ) where

import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.PatternMatcher.AST as PM

data DEADCODE

instance Stage DEADCODE where
  type StageId DEADCODE = 600

type Module = StdModule TopLevel

type TopLevel = PM.StdTopLevel DEADCODE
type Defn = StdDefn DEADCODE
type Expr = StdExpr DEADCODE
type Case = StdCase DEADCODE
