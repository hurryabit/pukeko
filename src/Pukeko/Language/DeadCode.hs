module Pukeko.Language.DeadCode
  ( DC.Module
  , cleanModule
  )
where

import           Control.Lens
import qualified Data.Graph    as G
import qualified Data.Set      as Set
import qualified Data.Set.Lens as Set

import           Pukeko.Language.AST.Classes
import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.DeadCode.AST       as DC
import qualified Pukeko.Language.PatternMatcher.AST as PM
import qualified Pukeko.Language.Ident              as Id

cleanModule :: PM.Module -> DC.Module
cleanModule = over module2tops $ \tops0 ->
  let (g, out, in_) = G.graphFromEdges $ map (\t -> (t, t^.lhs, deps t)) tops0
      reach = Set.fromList
              $ map (view _2 . out) $ maybe [] (G.reachable g) (in_ Id.main)
      keep t = (t^.lhs) `Set.member` reach
  in  map (over PM.topLevel2expr retagExpr) $ filter keep tops0
  where
    deps = Set.toList . Set.setOf (PM.topLevel2expr . traverse)
