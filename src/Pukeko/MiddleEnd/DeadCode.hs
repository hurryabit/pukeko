module Pukeko.MiddleEnd.DeadCode
  ( cleanModule
  )
where

import Pukeko.Prelude

import qualified Data.Graph    as G
import qualified Data.Map      as Map
import qualified Data.Set      as Set

import           Pukeko.AST.SuperCore
import qualified Pukeko.AST.Identifier as Id
import qualified Pukeko.MiddleEnd.CallGraph as CG

cleanModule :: Module -> Module
cleanModule mod0@(MkModule types extns supcs) =
  let g = CG.makeCallGraph mod0
      reach = Set.fromList
              $ map ((^. func2name.lctd) . CG.toDecl g)
              $ maybe [] (G.reachable (CG.graph g)) (CG.fromEVar g Id.main)
      clean :: Map Id.EVar a -> Map Id.EVar a
      clean = Map.filterWithKey (\z _ -> z `Set.member` reach)
  in  MkModule types (clean extns) (clean supcs)
