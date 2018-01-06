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
import           Pukeko.Language.Type               (Type)

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
topLevelLhs = view lhs . \case
  TLDef b _ -> b
  TLAsm b _ -> b

topLevel2expr ::
  (St.HasTLDef st ~ 'True, St.StageType st ~ Type) =>
  Traversal (TopLevel In) (TopLevel st) (Expr In Void Id.EVar) (Expr st Void Id.EVar)
topLevel2expr f = \case
  TLDef b e -> TLDef (retagBind b) <$> f e
  TLAsm b s -> pure (TLAsm (retagBind b) s)
