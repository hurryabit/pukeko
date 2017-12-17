{-# LANGUAGE DataKinds #-}
module Pukeko.Language.DeadCode.AST
  ( TypeCon
  , ExprCon
  , Module
  , PM.StdTopLevel (..)
  , TopLevel
  , Defn
  , Expr
  , Case
  ) where

import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.PatternMatcher.AST as PM

data DEADCODE

type TypeCon = PM.TypeCon
type ExprCon = PM.ExprCon

instance Stage DEADCODE where
  type ExprConOf DEADCODE = ExprCon
  type HasLam    DEADCODE = 'True
  type HasMat    DEADCODE = 'False

type Module = [TopLevel]

type TopLevel = PM.StdTopLevel DEADCODE
type Defn = StdDefn DEADCODE
type Expr = StdExpr DEADCODE
type Case = StdCase DEADCODE
