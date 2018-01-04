{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Pukeko.Language.AST.ModuleInfo
  ( Present (..)
  , GenModuleInfo (..)
  , getDCon
  , getTCon
  ) where

import qualified Data.Map                    as Map

import qualified Pukeko.Language.AST.ConDecl as Con
import qualified Pukeko.Language.Ident       as Id

data Present (b :: Bool) a where
  Present :: {unPresent :: a} -> Present 'True  a
  Absent  ::      Present 'False a

data GenModuleInfo cons = MkModuleInfo
  { tcons :: Present cons (Map.Map Id.TCon Con.TConDecl)
  , dcons :: Present cons (Map.Map Id.DCon Con.DConDecl)
  }

getTCon :: Id.TCon -> GenModuleInfo 'True -> Con.TConDecl
getTCon tcon = (Map.! tcon) . unPresent . tcons

getDCon :: Id.DCon -> GenModuleInfo 'True -> Con.DConDecl
getDCon tcon = (Map.! tcon) . unPresent . dcons
