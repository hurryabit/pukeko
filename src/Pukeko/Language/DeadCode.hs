module Pukeko.Language.DeadCode
  ( Module
  , cleanModule
  )
where

import           Control.Lens
import qualified Data.Graph    as G
import qualified Data.Set      as Set
import qualified Data.Set.Lens as Set

import           Pukeko.Language.Base.AST
import           Pukeko.Language.DeadCode.AST
import qualified Pukeko.Language.PatternMatcher.AST as In
import qualified Pukeko.Language.Ident              as Id

cleanModule :: In.Module -> Module
cleanModule module_ =
  let (g, out, in_) = G.graphFromEdges $ map (\t -> (t, t^.lhs, deps t)) module_
      reach = Set.fromList
              $ map (view _2 . out) $ maybe [] (G.reachable g) (in_ Id.main)
      keep t = (t^.lhs) `Set.member` reach
  in  filter keep module_
  where
    deps = Set.toList . Set.setOf (In.topLevelExpr . traverse)
