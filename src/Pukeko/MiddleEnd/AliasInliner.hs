module Pukeko.MiddleEnd.AliasInliner
  ( inlineModule
  ) where

import Pukeko.Prelude hiding (join)

import           Control.Monad.ST
import qualified Data.Array        as A
import qualified Data.Array.ST     as A
import qualified Data.Map          as Map
import qualified Data.Set          as Set

import           Pukeko.AST.SystemF
import qualified Pukeko.AST.Identifier as Id

inlineModule :: Module st -> Module st
inlineModule (MkModule decls0) =
  let ls = mapMaybe (declLink . unlctd) decls0
      uf = unionFind ls
      decls1 = over
        (unWhere (traverse . lctd . decl2eval))
        (\x -> Map.findWithDefault x x uf) decls0
  in  MkModule decls1

declLink :: Decl st -> Maybe (Id.EVar, Id.EVar)
declLink = \case
  DDefn (MkDefn b (EVal x)) -> Just (b^.bind2evar, x)
  DSupC (MkSupCDecl z vs _ xs (EVal x))
    | null vs && null xs -> Just (z, x)
  _ -> Nothing

data UnionFind st a = UnionFind
  { indices :: Map a Int
  , names   :: A.Array Int a
  , links   :: A.STArray st Int Int
  }

newUnionFind :: Ord a => Set a -> ST s (UnionFind s a)
newUnionFind xs = do
  let is = Map.fromList (zip (toList xs) [0..])
  let bnds = (0, Map.size is - 1)
  let ns = A.array bnds (map (\(x, i) -> (i, x)) (Map.toList is))
  ls <- A.newListArray bnds [0..]
  pure (UnionFind is ns ls)

root :: UnionFind s a -> Int -> ST s Int
root uf i = do
  j <- A.readArray (links uf) i
  if i /= j
    then do
      k <- A.readArray (links uf) j
      A.writeArray (links uf) i k
      root uf j
    else
      pure i

join' :: UnionFind s a -> Int -> Int -> ST s ()
join' uf i j = do
  l <- root uf j
  A.writeArray (links uf) i l

join :: Ord a => UnionFind s a -> a -> a -> ST s ()
join uf x y = join' uf (indices uf ! x) (indices uf ! y)
  where (!) = flip (Map.findWithDefault (bug "join"))

unionFind :: Ord a => [(a, a)] -> Map a a
unionFind xys = runST $ do
  let xs = foldMap (\(x, y) -> Set.singleton x <> Set.singleton y) xys
  uf <- newUnionFind xs
  for_ xys (uncurry (join uf))
  traverse (\i -> (names uf A.!) <$> root uf i) (indices uf)
