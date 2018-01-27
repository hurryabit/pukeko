module Pukeko.MiddleEnd.Prettifier
  ( prettifyModule
  ) where

import Pukeko.Prelude

import qualified Data.Map      as Map
import qualified Data.Set      as Set

import           Pukeko.AST.SystemF
import qualified Pukeko.AST.Identifier as Id

prettifyModule :: Module st -> Module st
prettifyModule (MkModule tops0) =
  let xs = setOf (traverse . traverse . decl2func) tops0
      mp = cluster xs
      rename x = Map.findWithDefault x x mp
      tops1 = over (unWhere (traverse . lctd . decl2func)) rename tops0
      tops2 = over (unWhere (traverse . lctd . decl2eval)) rename tops1
  in  MkModule tops2

cluster :: Set Id.EVar -> Map Id.EVar Id.EVar
cluster xs0 =
  let mp =
        foldl
          (\acc x -> Map.insertWith Set.union (Id.stripPart x) (Set.singleton x) acc)
          Map.empty xs0
      f y xs1
        | not (y `Set.member` xs1) = Map.singleton (Set.findMax xs1) y
        | otherwise = Map.empty
  in  ifoldMap f mp
