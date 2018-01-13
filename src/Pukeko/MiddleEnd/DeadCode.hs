{-# LANGUAGE ConstraintKinds #-}
module Pukeko.MiddleEnd.DeadCode
  ( cleanModule
  )
where

import           Control.Lens
import qualified Data.Graph    as G
import           Data.Maybe    (mapMaybe)
import qualified Data.Set      as Set
import qualified Data.Set.Lens as Set

import           Pukeko.AST.SystemF
import qualified Pukeko.AST.Identifier as Id

cleanModule :: Module st -> Module st
cleanModule (MkModule tops0) =
  let edges t = fmap (\lhs -> (t, lhs, deps t)) (firstOf top2lhs t)
      (g, out, in_) = G.graphFromEdges $ mapMaybe edges tops0
      reach = Set.fromList
              $ map (view _2 . out) $ maybe [] (G.reachable g) (in_ Id.main)
      keep = (`Set.member` reach)
      tops1 = filter (maybe True keep . firstOf top2lhs) tops0
  in  MkModule tops1
  where
    deps = Set.toList . Set.setOf top2eval
