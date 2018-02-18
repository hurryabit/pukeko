module Pukeko.MiddleEnd.DeadCode
  ( cleanModule
  )
where

import Pukeko.Prelude

import qualified Data.Graph    as G
import qualified Data.Map      as Map
import qualified Data.Set      as Set

import           Pukeko.AST.Name
import           Pukeko.AST.SuperCore
import qualified Pukeko.MiddleEnd.CallGraph as CG

cleanModule :: Module -> Module
cleanModule mod0@(MkModule types extns supcs) =
  let [main] = filter (\name -> nameText name == "main") (Map.keys supcs)
      g = CG.makeCallGraph mod0
      reach = Set.fromList
              $ map (nameOf . CG.toDecl g)
              $ maybe [] (G.reachable (CG.graph g)) (CG.fromEVar g main)
      clean :: Map (Name EVar) a -> Map (Name EVar) a
      clean = Map.filterWithKey (\z _ -> z `Set.member` reach)
  in  MkModule types (clean extns) (clean supcs)
