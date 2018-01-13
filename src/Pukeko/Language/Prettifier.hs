module Pukeko.Language.Prettifier
  ( prettifyModule
  ) where

import           Control.Lens
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Set.Lens as Set

import           Pukeko.Language.AST.Std
import           Pukeko.Language.AST.Stage
import           Pukeko.Language.AST.ModuleInfo (info2funs)
import qualified Pukeko.Language.Ident          as Id

prettifyModule :: (HasTLTyp st ~ 'False, HasTLVal st ~ 'False) => Module st -> Module st
prettifyModule (MkModule info0 tops0) =
  let xs = Set.setOf (traverse . top2lhs) tops0
      mp = cluster xs
      rename x = Map.findWithDefault x x mp
      info1 = over info2funs (Map.mapKeys rename) info0
      tops1 = over (traverse . top2lhs) rename tops0
      tops2 = over (traverse . top2eval) rename tops1
  in  MkModule info1 tops2

cluster :: Set.Set Id.EVar -> Map.Map Id.EVar Id.EVar
cluster xs0 =
  let mp =
        foldl
          (\acc x -> Map.insertWith Set.union (Id.stripPart x) (Set.singleton x) acc)
          Map.empty xs0
      f y xs1
        | not (y `Set.member` xs1) = Map.singleton (Set.findMax xs1) y
        | otherwise = Map.empty
  in  ifoldMap f mp
