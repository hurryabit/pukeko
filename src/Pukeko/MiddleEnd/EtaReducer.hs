{-# LANGUAGE ViewPatterns #-}
module Pukeko.MiddleEnd.EtaReducer
  ( reduceModule
  ) where

import Pukeko.Prelude

import           Control.Lens (nullOf)

import           Pukeko.AST.Expr (_TmPar)
import           Pukeko.AST.Name
import           Pukeko.AST.SuperCore
import           Pukeko.AST.Type

reduceModule :: Module -> Module
reduceModule = over (mod2supcs . traverse) erSupCDecl

-- | Takes a list of abstractions in reverse order (the innermost abstraction
-- goes first) and an expression and performs eta reduction as long as possible.
etaReduce :: ([Par], Expr) -> ([Par], Expr)
etaReduce = \case
  (TmPar (x, _):ps, ETmApp e (EVar y)) | x == y -> etaReduce (ps, e)
  (TyPar  v    :ps, ETyApp e (TVar w)) | v == w -> etaReduce (ps, e)
  (ps, e) -> (ps, e)

erSupCDecl :: FuncDecl (Only SupC) -> FuncDecl (Only SupC)
erSupCDecl supc@(SupCDecl z t ps0 e0)
  | nullOf (traverse . _TmPar) ps1 = (SupCDecl z t (reverse ps1) e1)
  | otherwise                      = supc
  where
    (ps1, e1) = etaReduce (reverse ps0, e0)
