module Pukeko.MiddleEnd.Prettifier
  ( prettifyModule
  ) where

import Pukeko.Prelude

-- import           Control.Lens
import qualified Data.Map      as Map
import qualified Data.Set      as Set

import           Pukeko.AST.SystemF
import qualified Pukeko.AST.Identifier as Id

prettifyModule :: Module st -> Module st
prettifyModule (MkModule tops0) =
  let xs = setOf (traverse . top2lhs) tops0
      mp = cluster xs
      rename x = Map.findWithDefault x x mp
      tops1 = over (traverse . top2lhs) rename tops0
      tops2 = over (traverse . top2eval) rename tops1
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
