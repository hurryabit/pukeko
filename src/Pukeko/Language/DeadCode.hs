module Pukeko.Language.DeadCode
  ( cleanModule
  )
where

import           Control.Lens
import qualified Data.Graph    as G
import qualified Data.Set      as Set
import qualified Data.Set.Lens as Set

import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.AST.Stage          as St
import qualified Pukeko.Language.Ident              as Id

type In  = St.PatternMatcher
type Out = St.DeadCode

cleanModule :: Module In -> Module Out
cleanModule = over module2tops $ \tops0 ->
  let (g, out, in_) = G.graphFromEdges $ map (\t -> (t, topLevelLhs t, deps t)) tops0
      reach = Set.fromList
              $ map (view _2 . out) $ maybe [] (G.reachable g) (in_ Id.main)
      keep t = (topLevelLhs t) `Set.member` reach
  in  map (over topLevel2expr retagExpr) $ filter keep tops0
  where
    deps = Set.toList . Set.setOf (topLevel2expr . traverse)

topLevelLhs :: TopLevel In -> Id.EVar
topLevelLhs = \case
  TLDef _ x _ -> x
  TLAsm _ x _ -> x

topLevel2expr ::
  St.HasTLDef st ~ 'True =>
  Traversal (TopLevel In) (TopLevel st) (Expr In Id.EVar) (Expr st Id.EVar)
topLevel2expr f = \case
  TLDef w x e -> TLDef w x <$> f e
  TLAsm w x s -> pure (TLAsm w x s)
