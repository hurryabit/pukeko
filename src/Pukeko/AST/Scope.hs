{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
-- | Type for encoding DeBruijn style scoping.
module Pukeko.AST.Scope
  ( Scope
  , TScope
  , scope
  , mkBound
  , strengthenScope0
  , weakenScope
  , instantiateN
  , abstract
  , (>>>=)
  )
  where

import Pukeko.Prelude

import           Data.Forget
import qualified Data.Vector       as Vec
import           Data.Aeson.TH

import           Pukeko.AST.Name

-- NOTE: The order of the constructors is chosen such that the derived @Ord@
-- instance sorts the corresponding de Bruijn indices in decreasing order, which
-- is good for lambda lifting.
data Scope b i v
  = Free v
  | Bound i (Forget b)
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

type TScope = Scope (Name TVar)

scope :: (v -> a) -> (i -> a) -> Scope b i v -> a
scope f g = scope' f (const . g)

scope' :: (v -> a) -> (i -> b -> a) -> Scope b i v -> a
scope' f g = \case
  Free  x            -> f x
  Bound i (Forget b) -> g i b

mkBound :: i -> b -> Scope b i v
mkBound i b = Bound i (Forget b)

strengthenScopeEither :: Scope b i v -> Either (i, b) v
strengthenScopeEither = scope' Right (\i b -> Left (i, b))

strengthenScope0 :: (HasCallStack, Show b) => Scope b Int v -> v
strengthenScope0 = either (bugWith "strengthenScope0") id . strengthenScopeEither

weakenScope :: v -> Scope b i v
weakenScope = Free

instantiate :: Monad f => (i -> f v) -> f (Scope b i v) -> f v
instantiate f e = e >>= scope pure f

instantiateN :: (HasCallStack, Monad f, Foldable t) =>
  t (f v) -> f (Scope b Int v) -> f v
instantiateN xsL = instantiate lk
  where xsV = Vec.fromList (toList xsL)
        lk i = maybe (bugWith "instantiateN" i) id (xsV Vec.!? i)

abstract :: (v -> Maybe (i, b)) -> v -> Scope b i v
abstract f x = maybe (Free x) (uncurry mkBound) (f x)

dist :: Applicative f => Scope b i (f v) -> f (Scope b i v)
dist = \case
  Bound i b -> pure (Bound i b)
  Free  t   -> fmap Free t

(>>>=) :: Monad f => f (Scope b i v1) -> (v1 -> f v2) -> f (Scope b i v2)
t >>>= f = t >>= dist . fmap f

_Free :: Prism (Scope b i v1) (Scope b i v2) v1 v2
_Free = prism Free $ \case
  Free x -> Right x
  Bound i b -> Left (Bound i b)

_Bound :: Prism (Scope b i1 v) (Scope b i2 v) (i1, b) (i2, b)
_Bound = prism (uncurry mkBound) $ \case
  Free x -> Left (Free x)
  Bound i (Forget b) -> Right (i, b)

data Pair f g a = Pair (f a) (g a)
  deriving (Functor)

instance Bifunctor (Scope b) where
  bimap f g = \case
    Bound i b -> Bound (f i) b
    Free  x   -> Free  (g x)

deriveToJSON defaultOptions ''Scope
