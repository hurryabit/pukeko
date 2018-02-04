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
import           Pukeko.AST.SystemF
import           Pukeko.AST.Stage


type St = LambdaLifter

data FuncDecl
  = FSupC (SupCDecl St)
  | FExtn (ExtnDecl (StType St))

data CallGraph = CallGraph
  { graph    :: G.Graph
  , toDecl   :: G.Vertex -> FuncDecl
  , fromEVar :: Id.EVar -> Maybe G.Vertex
  }

fdecl2evar :: FuncDecl -> Id.EVar
fdecl2evar = \case
  FSupC supc -> supc^.supc2func.lctd
  FExtn extn -> extn^.extn2bind.bind2evar.lctd

fdecl2decl :: FuncDecl -> Decl St
fdecl2decl = \case
  FSupC supc -> DSupC supc
  FExtn extn -> DExtn extn

makeCallGraph :: Module St -> CallGraph
makeCallGraph (MkModule decls) = CallGraph g ((^._1) . f) n
  where
    (g, f, n) = G.graphFromEdges (mapMaybe deps decls)
    deps :: Decl St -> Maybe (FuncDecl, Id.EVar, [Id.EVar])
    deps = \case
      DSupC decl@(MkSupCDecl (unlctd -> z) _ _ _ e) ->
        Just (FSupC decl, z, toList (setOf (expr2atom . _AVal) e))
      DExtn decl ->
        Just (FExtn decl, decl^.extn2bind.bind2evar.lctd, [])
      _ -> Nothing

scc :: CallGraph -> [G.SCC FuncDecl]
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
          FSupC _ -> D.Ellipse
          FExtn _ -> D.BoxShape
    D.node v [D.Label (D.StrLabel (T.pack (Id.name (fdecl2evar decl)))), D.Shape shape]
    traverse (v D.-->) us
