{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
module Pukeko.AST.ModuleInfo
  ( Present (..)
  , GenModuleInfo (..)
  , tcons
  , dcons
  , funs
  , info2funs
  ) where

import           Control.Lens

import qualified Data.Map as Map

import           Pukeko.AST.Pos
import           Pukeko.AST.ConDecl    (Some1 (..), Pair1 (..))
import qualified Pukeko.AST.ConDecl    as Con
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Type

data Present (b :: Bool) a where
  Present :: {unPresent :: a} -> Present 'True  a
  Absent  ::      Present 'False a

data GenModuleInfo cons vals = MkModuleInfo
  { _info2tcons :: Present cons (Map.Map Id.TCon (Some1 Con.TConDecl))
  , _info2dcons :: Present cons (Map.Map Id.DCon (Some1 (Pair1 Con.TConDecl Con.DConDecl)))
  , _info2funs  :: Present vals (Map.Map Id.EVar (Pos, Type Void))
  }

tcons :: GenModuleInfo 'True vals -> Map.Map Id.TCon (Some1 Con.TConDecl)
tcons = unPresent . _info2tcons

dcons :: GenModuleInfo 'True vals -> Map.Map Id.DCon (Some1 (Pair1 Con.TConDecl Con.DConDecl))
dcons = unPresent . _info2dcons

funs :: GenModuleInfo cons 'True -> Map.Map Id.EVar (Pos, Type Void)
funs = unPresent . _info2funs

info2funs :: Traversal' (GenModuleInfo cons vals) (Map.Map Id.EVar (Pos, Type Void))
info2funs f (MkModuleInfo ts ds fs) = MkModuleInfo ts ds <$> traverse f fs

deriving instance Functor     (Present b)
deriving instance Foldable    (Present b)
deriving instance Traversable (Present b)
