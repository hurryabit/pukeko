{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
module Data.List.Sized
  ( Nat (..)
  , One
  , List (..)
  , pattern Singleton
  , SomeList (..)
  , fromList
  , withList
  , match
  , map
  , (++)
  , zip
  , zipWith
  , unzip
  , unzipWith
  , transpose
  )
where

import           Prelude        hiding ((++), map, replicate, unzip, zip, zipWith)
import           Data.Bifunctor (bimap)

data Nat = Zero | Succ Nat

type One = 'Succ 'Zero

type family (+) (m :: Nat) (n :: Nat) :: Nat

type instance (+) 'Zero     n = n
type instance (+) ('Succ m) n = 'Succ (m+n)

data List (n :: Nat) a where
  Nil  ::                  List  'Zero    a
  Cons :: a -> List n a -> List ('Succ n) a

pattern Singleton :: a -> List One a
pattern Singleton x = Cons x Nil

data SomeList a where
   SomeList :: List n a -> SomeList a

fromList :: forall a. [a] -> SomeList a
fromList = \case
  []   -> SomeList Nil
  x:xs ->
    case fromList xs of
      SomeList ys -> SomeList (Cons x ys)

withList :: [a] -> (forall n. List n a -> b) -> b
withList xs k = case fromList xs of
  SomeList ys -> k ys

match :: List n a -> [b] -> Maybe (List n b)
match Nil         []     = Just Nil
match (Cons _ xs) (y:ys) = fmap (Cons y) (match xs ys)
match _           _      = Nothing

map :: (a -> b) -> List n a -> List n b
map = fmap

(++) :: List m a -> List n a -> List (m + n) a
Nil       ++ ys = ys
Cons x xs ++ ys = Cons x (xs ++ ys)

zip :: List n a -> List n b -> List n (a, b)
zip = zipWith (,)

zipWith :: (a -> b -> c) -> List n a -> List n b -> List n c
zipWith _ Nil Nil = Nil
zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith f xs ys)

unzip :: List n (a, b) -> (List n a, List n b)
unzip = unzipWith id

unzipWith :: (a -> (b, c)) -> List n a -> (List n b, List n c)
unzipWith _ Nil = (Nil, Nil)
unzipWith f (Cons x xs) = bimap (Cons y) (Cons z) (unzipWith f xs)
  where (y, z) = f x


transpose :: List n b -> List m (List n a) -> List n (List m a)
transpose zs Nil           = map (const Nil) zs
transpose zs (Cons xs xss) = zipWith Cons xs (transpose zs xss)

deriving instance Functor (List n)
deriving instance Foldable (List n)
deriving instance Traversable (List n)

deriving instance Functor SomeList
deriving instance Foldable SomeList
deriving instance Traversable SomeList
