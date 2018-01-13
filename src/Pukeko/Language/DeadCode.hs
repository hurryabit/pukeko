{-# LANGUAGE ConstraintKinds #-}
module Pukeko.Language.DeadCode
  ( cleanModule
  )
where

import           Control.Lens
import qualified Data.Graph    as G
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Set.Lens as Set

import           Pukeko.Language.AST.Std
import           Pukeko.Language.AST.ModuleInfo (info2funs)
import qualified Pukeko.Language.AST.Stage      as St
import qualified Pukeko.Language.Ident          as Id

type ElimStage st = (St.HasTLTyp st ~ 'False, St.HasTLVal st ~ 'False)

cleanModule :: (ElimStage st) => Module st -> Module st
cleanModule (MkModule info0 tops0) =
  let (g, out, in_) = G.graphFromEdges $ map (\t -> (t, t^.top2lhs, deps t)) tops0
      reach = Set.fromList
              $ map (view _2 . out) $ maybe [] (G.reachable g) (in_ Id.main)
      keep = (`Set.member` reach)
      info1 = over info2funs (Map.filterWithKey (const . keep)) info0
      tops1 = filter (keep . view top2lhs) tops0
  in  MkModule info1 tops1
  where
    deps = Set.toList . Set.setOf top2eval
