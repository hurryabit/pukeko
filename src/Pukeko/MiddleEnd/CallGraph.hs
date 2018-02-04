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

import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Language
import           Pukeko.AST.SuperCore
import           Pukeko.AST.Expr.Optics
import           Pukeko.AST.Type

data CallGraph = CallGraph
  { graph    :: G.Graph
  , toDecl   :: G.Vertex -> FuncDecl 'Any
  , fromEVar :: Id.EVar -> Maybe G.Vertex
  }

makeCallGraph :: Module -> CallGraph
makeCallGraph mod0 = CallGraph g ((^._1) . f) n
  where
    (g, f, n) = G.graphFromEdges (supcs ++ extns)
    supcs = [ (castAny supc, z, toList (setOf (func2expr . expr2atom . _AVal) supc))
            | (z, supc) <- itoList (mod0^.mod2supcs)
            ]
    extns = [ (castAny extn, z, []) | (z, extn) <- itoList (mod0^.mod2extns) ]

scc :: CallGraph -> [G.SCC (FuncDecl 'Any)]
scc (CallGraph graph vertex_fn _) = map decode forest
  where
    forest = G.scc graph
    decode (G.Node v []) | mentions_itself v = G.CyclicSCC [vertex_fn v]
                         | otherwise         = G.AcyclicSCC (vertex_fn v)
    decode other = G.CyclicSCC (dec other [])
                 where
                   dec (G.Node v ts) vs = vertex_fn v : foldr dec vs ts
    mentions_itself v = v `elem` (graph A.! v)

renderCallGraph :: CallGraph -> T.Text
renderCallGraph g = D.renderDot . D.toDot . D.digraph' $ do
  ifor_ (graph g) $ \v us -> do
    let decl = toDecl g v
    let shape = case decl of
          SupCDecl{} -> D.Ellipse
          ExtnDecl{} -> D.BoxShape
    D.node v [D.Label (D.StrLabel (T.pack (Id.name (decl^.func2name)))), D.Shape shape]
    traverse (v D.-->) us
