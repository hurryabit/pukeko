{-# LANGUAGE DataKinds #-}
module Pukeko.Language.DeadCode
  ( DC.Module
  , cleanModule
  )
where

import           Control.Lens
import qualified Data.Graph    as G
import qualified Data.Set      as Set
import qualified Data.Set.Lens as Set

import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.DeadCode.AST       as DC
import qualified Pukeko.Language.PatternMatcher.AST as PM
import qualified Pukeko.Language.Ident              as Id

cleanModule :: PM.Module -> DC.Module
cleanModule = over module2tops $ \tops0 ->
  let (g, out, in_) = G.graphFromEdges $ map (\t -> (t, topLevelLhs t, deps t)) tops0
      reach = Set.fromList
              $ map (view _2 . out) $ maybe [] (G.reachable g) (in_ Id.main)
      keep t = (topLevelLhs t) `Set.member` reach
  in  map (over topLevel2expr retagExpr) $ filter keep tops0
  where
    deps = Set.toList . Set.setOf (topLevel2expr . traverse)

topLevelLhs :: PM.TopLevel -> Id.EVar
topLevelLhs = \case
  Def _ x _ -> x
  Asm _ x _ -> x

topLevel2expr ::
  HasDef st ~ 'True =>
  Traversal PM.TopLevel (StdTopLevel st) (PM.Expr Id.EVar) (StdExpr st Id.EVar)
topLevel2expr f = \case
  Def w x e -> Def w x <$> f e
  Asm w x s -> pure (Asm w x s)
