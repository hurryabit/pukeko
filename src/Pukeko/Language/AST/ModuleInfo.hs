{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Pukeko.Language.AST.ModuleInfo
  ( Present (..)
  , GenModuleInfo (..)
  , tcons
  , dcons
  , funs
  ) where

import qualified Data.Map                    as Map

import           Pukeko.Pos
import qualified Pukeko.Language.AST.ConDecl as Con
import qualified Pukeko.Language.Ident       as Id
import qualified Pukeko.Language.Type        as Ty

data Present (b :: Bool) a where
  Present :: {unPresent :: a} -> Present 'True  a
  Absent  ::      Present 'False a

data GenModuleInfo cons vals = MkModuleInfo
  { _tcons :: Present cons (Map.Map Id.TCon Con.TConDecl)
  , _dcons :: Present cons (Map.Map Id.DCon Con.DConDecl)
  , _funs  :: Present vals (Map.Map Id.EVar (Pos, Ty.Type Ty.Closed))
  }

tcons :: GenModuleInfo 'True vals -> Map.Map Id.TCon Con.TConDecl
tcons = unPresent . _tcons

dcons :: GenModuleInfo 'True vals -> Map.Map Id.DCon Con.DConDecl
dcons = unPresent . _dcons

funs :: GenModuleInfo cons 'True -> Map.Map Id.EVar (Pos, Ty.Type Ty.Closed)
funs = unPresent . _funs
