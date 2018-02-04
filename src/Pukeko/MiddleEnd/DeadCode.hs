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

cleanModule :: ModuleSC -> ModuleSC
cleanModule mod0@(MkModuleSC types extns supcs) =
  let g = CG.makeCallGraph mod0
      reach = Set.fromList
              $ map (CG.fdecl2evar . CG.toDecl g)
              $ maybe [] (G.reachable (CG.graph g)) (CG.fromEVar g Id.main)
      clean :: Map Id.EVar a -> Map Id.EVar a
      clean = Map.filterWithKey (\z _ -> z `Set.member` reach)
  in  MkModuleSC types (clean extns) (clean supcs)
