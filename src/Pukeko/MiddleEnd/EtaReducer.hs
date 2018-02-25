{-# LANGUAGE ViewPatterns #-}
module Pukeko.MiddleEnd.EtaReducer
  ( reduceModule
  ) where

import Pukeko.Prelude

import           Control.Lens (nullOf)

import           Pukeko.AST.Expr.Optics
import           Pukeko.AST.Name
import           Pukeko.AST.SuperCore
import           Pukeko.AST.Type

reduceModule :: Module -> Module
reduceModule = over (mod2supcs . traverse) erSupCDecl

_EVar :: Expr -> Maybe (Name EVar)
_EVar = \case
  EVar x -> Just x
  _      -> Nothing

_TVar :: GenType tv -> Maybe tv
_TVar = \case
  TVar v -> Just v
  _      -> Nothing

erSupCDecl :: FuncDecl (Only SupC) -> FuncDecl (Only SupC)
erSupCDecl = \case
  supc@(SupCDecl z tz vs0 bs0 e0) ->
    case e0 of
      -- Reduce expression parameters:
      EApp{}
        | (e1, traverse _EVar -> Just xs1) <- unwindEApp e0
        , nullOf freeEVar e1
        , map nameOf bs0 == xs1 ->
          erSupCDecl (SupCDecl z tz vs0 [] e1)
      -- Reduce type paramaters:
      ETyApp e1 (traverse _TVar -> Just vs1)
        | nullOf freeEVar e1
        , null bs0
          -- FIXME: The condition below /should/ check whether there are type
          -- variables which are ultimately free in @e1@. However, it's actually
          -- checking for types which contain free type variables. This is more
          -- restrictive since the binding effect of type abstractions is
          -- ignored. Being too restricitive is ok for now but not in the long
          -- run.
        , nullOf (expr2type . traverse) e1
        , length vs0 == length vs1
        , vs0 == toList vs1 ->
          SupCDecl z tz [] [] e1
      -- Promote type abstraction when the super combinator doesn't contain any
      -- abstractions itself:
      ETyAbs vs1 e1
        | null vs0 && null bs0
        , TUni (length -> n) _ <- tz
          -- NOTE: The check @length vs == n@ is only necessary because our type
          -- checker distinguishes between the types @∀a b. t[a, b]@ and
          -- @∀a. ∀b. t[a, b]@, which is clearly a problem.
        , length vs1 == n ->
            erSupCDecl (SupCDecl z tz (toList vs1) [] e1)
      _ -> supc
