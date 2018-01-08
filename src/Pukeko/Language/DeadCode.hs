{-# LANGUAGE ConstraintKinds #-}
module Pukeko.Language.DeadCode
  ( cleanModule
  )
where

import           Control.Lens
import qualified Data.Graph    as G
import qualified Data.Set      as Set
import qualified Data.Set.Lens as Set

import           Pukeko.Language.AST.Classes
import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.AST.Stage          as St
import qualified Pukeko.Language.Ident              as Id

-- TODO: Generalize type over arbitrary stages.
type ElimStage st = (st ~ St.PatternMatcher)

-- FIXME: Remove unreachable funs from module info as well.
cleanModule :: (ElimStage st) => Module st -> Module st
cleanModule = over module2tops $ \tops0 ->
  let (g, out, in_) = G.graphFromEdges $ map (\t -> (t, topLevelLhs t, deps t)) tops0
      reach = Set.fromList
              $ map (view _2 . out) $ maybe [] (G.reachable g) (in_ Id.main)
      keep t = (topLevelLhs t) `Set.member` reach
  in  filter keep tops0
  where
    deps = Set.toList . Set.setOf (topLevel2expr . expr2eval)

topLevelLhs :: (ElimStage st) => TopLevel st -> Id.EVar
topLevelLhs = view lhs . \case
  TLDef (MkDefn b _) -> b
  TLAsm b _ -> b

topLevel2expr ::
  (ElimStage st) =>
  Traversal (TopLevel st) (TopLevel st) (Expr st Void Void) (Expr st Void Void)
topLevel2expr f = \case
  TLDef d -> TLDef <$> defn2rhs f d
  TLAsm b s -> pure (TLAsm b s)
