{-# LANGUAGE ViewPatterns #-}
module Pukeko.MiddleEnd.EtaReducer
  ( reduceModule
  ) where

import Pukeko.Prelude

import           Data.Bitraversable

import           Pukeko.AST.Name
import           Pukeko.AST.SuperCore
import           Pukeko.AST.Type

reduceModule :: Module -> Module
reduceModule = over (mod2supcs . traverse) erSupCDecl

_EVar :: Expr tv ev -> Maybe ev
_EVar = \case
  EVar x -> Just x
  _      -> Nothing

_TVar :: Type tv -> Maybe tv
_TVar = \case
  TVar v -> Just v
  _      -> Nothing

erSupCDecl :: FuncDecl (Only SupC) -> FuncDecl (Only SupC)
erSupCDecl = \case
  supc@(SupCDecl z tz vs0 bs0 e0) ->
    case e0 of
      -- Reduce expression parameters:
      EApp{}
        | (traverse strengthenScope -> Just e1, traverse _EVar -> Just xs1) <-
            unwindEApp e0
        , length bs0 == length xs1
        , iall (\i -> scope absurd (== i)) xs1 ->
          erSupCDecl (SupCDecl z tz vs0 [] (weakenE e1))
      -- Reduce type paramaters:
      ETyApp
        (bitraverse strengthenScope strengthenScope -> Just e1)
        (traverse _TVar -> Just vs1)
        | null bs0
        , length vs0 == length vs1
        , iall (\i -> scope absurd (== i)) vs1 ->
          SupCDecl z tz [] [] (bimap weakenScope weakenScope e1)
      -- Promote type abstraction when the super combinator doesn't contain any
      -- abstractions itself:
      ETyAbs vs1 e1
        | null vs0 && null bs0
        , TUni (length -> n) _ <- tz
          -- NOTE: The check @length vs == n@ is only necessary because our type
          -- checker distinguishes between the types @∀a b. t[a, b]@ and
          -- @∀a. ∀b. t[a, b]@, which is clearly a problem.
        , length vs1 == n ->
            let e2 = first (over _Free strengthenScope0) e1
            in  erSupCDecl (SupCDecl z tz (toList vs1) [] e2)
      _ -> supc
