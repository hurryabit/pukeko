{-# OPTIONS_GHC -Wno-unused-imports #-}
module Pukeko.MiddleEnd.CallGraph
  ( module Pukeko.MiddleEnd.CallGraph
  , G.SCC (..)
  , G.flattenSCC
  ) where

import Pukeko.Prelude

import Control.Lens
import qualified Data.Array as A
import qualified Data.Graph as G
import qualified Data.GraphViz.Attributes.Complete as D
import qualified Data.GraphViz.Printing            as D
import qualified Data.GraphViz.Types.Generalised   as D
import qualified Data.GraphViz.Types.Monadic       as D
import qualified Data.Text.Lazy as T
import           Data.Tuple.Extra (fst3)

import           Pukeko.AST.Language
import           Pukeko.AST.Name
import           Pukeko.AST.SuperCore
import           Pukeko.AST.Expr.Optics
import           Pukeko.AST.Type

data CallGraph m = CallGraph
  { graph    :: G.Graph
  , toDecl   :: G.Vertex -> FuncDecl m
  , fromEVar :: Name EVar -> Maybe G.Vertex
  }

makeCallGraph' :: Foldable t => t (FuncDecl m) -> CallGraph m
makeCallGraph' decls = CallGraph g (fst3 . f) n
  where
    (g, f, n) = G.graphFromEdges (map deps (toList decls))
    deps decl =
      (decl, nameOf decl, toList (setOf (func2expr . expr2atom . _AVal) decl))

makeCallGraph :: Module -> CallGraph Any
makeCallGraph (MkModule _types extns supcs) =
  makeCallGraph' (fmap castAny extns <> fmap castAny supcs)

scc :: CallGraph m -> [G.SCC (FuncDecl m)]
scc (CallGraph graph vertex_fn _) = map decode forest
  where
    -- TODO: Simplify.
    forest = G.scc graph
    decode (G.Node v []) | mentions_itself v = G.CyclicSCC [vertex_fn v]
                         | otherwise         = G.AcyclicSCC (vertex_fn v)
    decode other = G.CyclicSCC (dec other [])
                 where
                   dec (G.Node v ts) vs = vertex_fn v : foldr dec vs ts
    mentions_itself v = v `elem` (graph A.! v)

renderCallGraph :: CallGraph m -> T.Text
renderCallGraph g = D.renderDot . D.toDot . D.digraph' $ do
  ifor_ (graph g) $ \v us -> do
    let decl = toDecl g v
    let shape = case decl of
          SupCDecl{} -> D.Ellipse
          ExtnDecl{} -> D.BoxShape
    D.node v
      [D.Label (D.StrLabel (T.pack (untag (nameText (nameOf decl))))), D.Shape shape]
    traverse (v D.-->) us
