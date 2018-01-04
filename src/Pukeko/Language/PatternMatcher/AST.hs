{-# LANGUAGE DataKinds #-}
module Pukeko.Language.PatternMatcher.AST
  ( Module
  , StdTopLevel (..)
  , TopLevel
  , Defn
  , Expr
  , Case

  , topLevel2expr
  ) where

import           Control.Lens (IndexedTraversal, indexed)

import           Pukeko.Language.AST.Classes
import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.Ident           as Id

data PATTERNMATCHER

instance Stage PATTERNMATCHER where
  type StageId PATTERNMATCHER = 500

type Module = StdModule TopLevel

data StdTopLevel st
  = Def Pos Id.EVar (StdExpr st Id.EVar)
  | Asm Pos Id.EVar String

type TopLevel = StdTopLevel PATTERNMATCHER

type Defn = StdDefn PATTERNMATCHER
type Expr = StdExpr PATTERNMATCHER
type Case = StdCase PATTERNMATCHER

topLevel2expr ::
  IndexedTraversal Pos
  (StdTopLevel st1) (StdTopLevel st2) (StdExpr st1 Id.EVar) (StdExpr st2 Id.EVar)
topLevel2expr f = \case
  Def w x t -> Def w x <$> indexed f w t
  Asm w x s -> pure $ Asm w x s

instance HasLhs (StdTopLevel st) where
  type Lhs (StdTopLevel st) = Id.EVar
  lhs f = \case
    Def w x t -> fmap (\x' -> Def w x' t) (f x)
    Asm w x s -> fmap (\x' -> Asm w x' s) (f x)
