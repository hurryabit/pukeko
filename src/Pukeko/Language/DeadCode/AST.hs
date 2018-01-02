{-# LANGUAGE DataKinds #-}
module Pukeko.Language.DeadCode.AST
  ( TCon
  , DCon
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

type TCon = PM.TCon
type DCon = PM.DCon

instance Stage DEADCODE where
  type DConRef DEADCODE = DCon
  type HasLam  DEADCODE = 'True
  type HasMat  DEADCODE = 'False

type Module = [TopLevel]

type TopLevel = PM.StdTopLevel DEADCODE
type Defn = StdDefn DEADCODE
type Expr = StdExpr DEADCODE
type Case = StdCase DEADCODE
