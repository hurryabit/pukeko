{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Bilens where

import Control.Applicative
import Control.Lens.Indexed
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Coerce
import Data.Function
import Data.Functor.Identity
import Data.Monoid
import Data.Profunctor.Unsafe

type Bitraversal t1 t2 a1 a2 b1 b2 =
  forall f. Applicative f => (a1 -> f a2) -> (b1 -> f b2) -> t1 -> f t2

type IndexedBitraversal i j t1 t2 a1 a2 b1 b2 =
  forall p q f. (Indexable i p, Indexable j q, Applicative f) =>
  p a1 (f a2) -> q b1 (f b2) -> t1 -> f t2

class Bifunctor p => BifunctorWithIndex i p | p -> i where
  ibimap  :: (i -> a1 -> a2) -> (i -> b1 -> b2) -> p a1 b1 -> p a2 b2
  ifirst  :: (i -> a1 -> a2)                    -> p a1 b  -> p a2 b
  isecond ::                    (i -> b1 -> b2) -> p a  b1 -> p a  b2

  default ibimap ::
    forall a1 a2 b1 b2. (BitraversableWithIndex i p) =>
    (i -> a1 -> a2) -> (i -> b1 -> b2) -> p a1 b1 -> p a2 b2
  ibimap = coerce
    (ibitraverse ::
        (i -> a1 -> Identity a2) -> (i -> b1 -> Identity b2) ->
        p a1 b1 -> Identity (p a2 b2))

  ifirst  f = ibimap f (const id)
  isecond g = ibimap (const id) g

class Bifoldable p => BifoldableWithIndex i p | p -> i where
  ibifoldMap :: Monoid m => (i -> a -> m) -> (i -> b -> m) -> p a b -> m
  ibifoldr   :: (i -> a -> c -> c) -> (i -> b -> c -> c) -> c -> p a b -> c

  default ibifoldMap ::
    forall m a b. (BitraversableWithIndex i p, Monoid m) =>
    (i -> a -> m) -> (i -> b -> m) -> p a b -> m
  ibifoldMap = coerce
    (ibitraverse ::
        (i -> a -> Const m ()) -> (i -> b -> Const m ()) -> p a b -> Const m (p () ()))

  ibifoldr f g z xys = appEndo (ibifoldMap (\i -> Endo . f i) (\i -> Endo . g i) xys) z

class (Bitraversable p, BifoldableWithIndex i p, BifunctorWithIndex i p) =>
  BitraversableWithIndex i p | p -> i where
  ibitraverse ::
    Applicative f => (i -> a1 -> f a2) -> (i -> b1 -> f b2) -> p a1 b1 -> f (p a2 b2)

firstOf ::
  Applicative f => (p a (f b) -> (c -> f c) -> (s -> f t)) -> p a (f b) -> s -> f t
firstOf l f = l f pure

secondOf ::
  Applicative f => ((a -> f a) -> q c (f d) -> (s -> f t)) -> q c (f d) -> s -> f t
secondOf l = l pure

bimapOf ::
  (Profunctor p, Profunctor q) =>
  (p a (Identity b) -> q c (Identity d) -> s -> Identity t) -> p a b -> q c d -> s -> t
bimapOf l f g = runIdentity #. l (Identity #. f) (Identity #. g)
