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

import           Pukeko.AST.SystemF
import qualified Pukeko.AST.Identifier as Id

-- | Follow all chains of links in a module and adjust all call sites accordingly.
inlineModule :: Module st -> Module st
inlineModule = over module2decls inlineDecls

-- | Follow all chains of links in a group of declarations and adjust all call
-- sites within this group.
inlineDecls :: [Decl st] -> [Decl st]
inlineDecls decls0 =
  let ls = mapMaybe declLink decls0
      uf = unionFind ls
  in  over (traverse . decl2atom . _AVal) (\x -> Map.findWithDefault x x uf) decls0

-- | Determine if a declaration is a link, i.e., of the form
--
-- > f = g
--
-- If it is, return the pair @(f, g)@.
declLink :: Decl st -> Maybe (Id.EVar, Id.EVar)
declLink = \case
  DDefn (MkDefn b (EVal x)) -> Just (b^.bind2evar.lctd, x)
  DSupC (MkSupCDecl z vs _ xs (EVal x))
    | null vs && null xs -> Just (z^.lctd, x)
  _ -> Nothing

-- | Run Tarjan's union find algorithm on a list of equivalences and return a
-- map from each element to its representative.
unionFind :: Ord a => [(a, a)] -> Map a a
unionFind xys = runST $ do
  let xs = foldMap (\(x, y) -> Set.singleton x <> Set.singleton y) xys
  ps <- sequence (Map.fromSet UF.fresh xs)
  for_ xys $ \(x, y) -> UF.union (ps Map.! x) (ps Map.! y)
  for ps $ \p -> UF.repr p >>= UF.descriptor
