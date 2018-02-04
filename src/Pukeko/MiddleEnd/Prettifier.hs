module Pukeko.MiddleEnd.Prettifier
  ( prettifyModule
  ) where

import Pukeko.Prelude

import qualified Data.Map      as Map
import qualified Data.Set      as Set

import           Pukeko.AST.SuperCore
import qualified Pukeko.AST.Identifier as Id

prettifyModule :: ModuleSC -> ModuleSC
prettifyModule = over modsc2supcs $ \supcs0 ->
  let xs :: Set Id.EVar
      xs = setOf (traverse . supc2func . lctd) supcs0
      mp = cluster xs
      rename x = Map.findWithDefault x x mp
  in  Map.fromList
      [ (z1, supc2)
      | (z0, supc0) <- itoList supcs0
      , let z1 = rename z0
      , let supc1 = set (supc2func . lctd) z1 supc0
      , let supc2 = over (supc2expr . expr2atom . _AVal) rename supc1
      ]

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
