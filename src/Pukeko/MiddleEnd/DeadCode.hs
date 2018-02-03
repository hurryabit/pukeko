module Pukeko.MiddleEnd.DeadCode
  ( cleanModule
  )
where

import Pukeko.Prelude

import           Control.Lens  (firstOf)
import qualified Data.Graph    as G
import qualified Data.Set      as Set

import           Pukeko.AST.SystemF
import qualified Pukeko.AST.Identifier as Id

cleanModule :: forall st. Module st -> Module st
cleanModule (MkModule tops0) =
  let edges t = fmap (\lhs -> (t, lhs, deps t)) (firstOf decl2func t)
      (g, out, in_) = G.graphFromEdges $ mapMaybe edges tops0
      reach = Set.fromList
              $ map ((\(_, l, _) -> l) . out) $ maybe [] (G.reachable g) (in_ Id.main)
      keep = (`Set.member` reach)
      tops1 :: [Decl st]
      tops1 = filter (maybe True keep . firstOf decl2func) tops0
  in  MkModule tops1
  where
    deps = Set.toList . setOf decl2eval
