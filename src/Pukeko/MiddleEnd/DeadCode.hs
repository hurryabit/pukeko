module Pukeko.MiddleEnd.DeadCode
  ( cleanModule
  )
where

import Pukeko.Prelude

import           Control.Lens  (firstOf)
import qualified Data.Graph    as G
import qualified Data.Set      as Set

import           Pukeko.AST.SystemF
import           Pukeko.AST.Stage
import qualified Pukeko.AST.Identifier as Id
import qualified Pukeko.MiddleEnd.CallGraph as CG

cleanModule :: Module LambdaLifter -> Module LambdaLifter
cleanModule mod0@(MkModule decls0) =
  let g = CG.makeCallGraph mod0
      reach = Set.fromList
              $ map (CG.fdecl2evar . CG.toDecl g)
              $ maybe [] (G.reachable (CG.graph g)) (CG.fromEVar g Id.main)
      keep = (`Set.member` reach)
      decls1 = filter (maybe True keep . firstOf decl2func) decls0
  in  MkModule decls1
