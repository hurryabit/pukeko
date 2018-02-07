module Pukeko.MiddleEnd.Prettifier
  ( prettifyModule
  ) where

import Pukeko.Prelude

import qualified Data.Map      as Map
import qualified Data.Set      as Set

import           Pukeko.AST.SuperCore
import           Pukeko.AST.Expr.Optics
import qualified Pukeko.AST.Identifier as Id

prettifyModule :: Module -> Module
prettifyModule = over mod2supcs $ \supcs0 ->
  let xs :: Set Id.EVar
      xs = setOf (traverse . func2name) supcs0
      mp = cluster xs
      rename x = Map.findWithDefault x x mp
  in  Map.fromList
      [ (z1, supc2)
      | (z0, supc0) <- itoList supcs0
      , let z1 = rename z0
      , let supc1 = set func2name z1 supc0
      , let supc2 = over (func2expr . expr2atom . _AVal) rename supc1
      ]

cluster :: Set Id.EVar -> Map Id.EVar Id.EVar
cluster xs0 =
  let mp =
        foldl
          (\acc x -> Map.insertWith Set.union x (Set.singleton x) acc)
          Map.empty xs0
      f y xs1
        | not (y `Set.member` xs1) = Map.singleton (Set.findMax xs1) y
        | otherwise = Map.empty
  in  ifoldMap f mp
