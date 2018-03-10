-- | Perform link compression for functions which are just aliases. For instance,
--
-- > f = Cons 1 Nil
-- > g = f
-- > h = g
-- > k = append g h
--
-- is transformed into
--
-- > f = Cons 1 Nil
-- > g = f
-- > h = f
-- > k = append f f
--
-- Notice that @g@ and @h@ are dead code now.
module Pukeko.MiddleEnd.AliasInliner where

import Pukeko.Prelude

import           Control.Monad.ST
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import qualified Data.UnionFind.ST as UF

import           Pukeko.AST.Name
import           Pukeko.AST.SuperCore
import           Pukeko.AST.Expr.Optics

-- | Follow all chains of links in a module and adjust all call sites accordingly.
inlineModule :: Module -> Module
inlineModule = over mod2supcs inlineSupCDecls

-- | Follow all chains of links in a group of declarations and adjust all call
-- sites within this group.
inlineSupCDecls :: Traversable t => t (FuncDecl (Only SupC)) -> t (FuncDecl (Only SupC))
inlineSupCDecls decls0 =
  let ls = mapMaybe isLink (toList decls0)
      uf = unionFind ls
  in  over (traverse . func2expr . expr2atom . _AVal)
        (\x -> Map.findWithDefault x x uf) decls0

-- | Determine if a declaration is a link, i.e., of the form
--
-- > f = g
--
-- If it is, return the pair @(f, g)@.
isLink :: FuncDecl (Only SupC) -> Maybe (TmVar, TmVar)
isLink = \case
  SupCDecl z _t ps (EVal x)
    | null ps -> Just (z, x)
  _           -> Nothing

-- | Run Tarjan's union find algorithm on a list of equivalences and return a
-- map from each element to its representative.
unionFind :: Ord a => [(a, a)] -> Map a a
unionFind xys = runST $ do
  let xs = foldMap (\(x, y) -> Set.singleton x <> Set.singleton y) xys
  ps <- sequence (Map.fromSet UF.fresh xs)
  for_ xys $ \(x, y) -> UF.union (ps Map.! x) (ps Map.! y)
  for ps (UF.repr >=> UF.descriptor)
