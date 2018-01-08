{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
-- | Type for encoding DeBruijn style scoping.
module Pukeko.Language.AST.Scope
  ( Scope (..)
  , EScope
  , EFinScope
  , TScope
  , TScope1
  , TFinScope
  , scope
  , scope'
  , _Bound
  , _Free
  , mkBound
  , strengthen
  , weaken
  , abstract1
  , unscope
  , dist
  , (>>>=)
  , extendEnv
  , HasEnvLevel (..)
  , HasEnv (..)
  , BaseEVar
  , BaseTVar
  , baseEVar
  , baseTVar
  , Void
  , absurd
  , Nat
  )
  where

import           Control.Lens
import           Data.Finite       (Finite)
import           Data.Forget
import qualified Data.Map          as Map
import qualified Data.Vector.Sized as Vec
import           Data.Void
import           GHC.TypeLits      (Nat)

import           Pukeko.Error      (bug)
import           Pukeko.Pretty
import qualified Pukeko.Language.Ident as Id

data Scope b i v
  = Bound i (Forget b)
  | Free v
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

type EScope = Scope Id.EVar

-- TODO: Replace @Ord@ by @Eq@.
type EFinScope n = EScope (Finite n)

type TScope = Scope Id.TVar

type TScope1 = TScope ()

type TFinScope n = TScope (Finite n)

scope :: (i -> a) -> (v -> a) -> Scope b i v -> a
scope f = scope' (const . f)

scope' :: (i -> b -> a) -> (v -> a) -> Scope b i v -> a
scope' f g = \case
  Bound i (Forget b) -> f i b
  Free  x            -> g x

mkBound :: i -> b -> Scope b i v
mkBound i b = Bound i (Forget b)

-- TODO: Use @HasCallstack@.
strengthen :: Show b => String -> Scope b i v -> v
strengthen component = \case
  Bound _ (Forget b) -> bug component "cannot strengthen" (Just (show b))
  Free  x            -> x

weaken :: v -> Scope b i v
weaken = Free

abstract1 :: (j -> Maybe i) -> Scope b j v -> Scope b j (Scope b i v)
abstract1 f = \case
  Bound (f -> Just i) b -> Free (Bound i b)
  Bound j             b -> Bound j b
  Free  x               -> Free (Free x)

-- TODO: Find out where we use the inverse 'scope' without naming it like this.
-- It's probably in the de Bruijn indexer.
unscope :: Scope Id.EVar i Id.EVar -> Id.EVar
unscope = \case
  Bound _ (Forget b) -> b
  Free  x            -> x

dist :: Applicative f => Scope b i (f v) -> f (Scope b i v)
dist = \case
  Bound i b -> pure (Bound i b)
  Free  t   -> fmap Free t

(>>>=) :: Monad f => f (Scope b i v1) -> (v1 -> f v2) -> f (Scope b i v2)
t >>>= f = t >>= dist . fmap f


lookupMap :: (Ord i, Pretty i) => i -> Map.Map i a -> a
lookupMap i = Map.findWithDefault (bug "scope" "lookup failed" (Just (prettyShow i))) i

data Pair f g a = Pair (f a) (g a)
  deriving (Functor)

extendEnv ::
  forall i v a b.
  (HasEnvLevel i, HasEnv v) =>
  EnvLevelOf i a ->
  EnvOf v a ->
  EnvOf (Scope b i v) a
extendEnv env_i env_v = Pair env_i env_v

class (Functor (EnvLevelOf i)) => HasEnvLevel i where
  type EnvLevelOf i :: * -> *
  lookupEnvLevel :: i -> EnvLevelOf i a -> a

instance HasEnvLevel Id.EVar where
  type EnvLevelOf Id.EVar = Map.Map Id.EVar
  lookupEnvLevel = lookupMap

instance HasEnvLevel (Finite n) where
  type EnvLevelOf (Finite n) = Vec.Vector n
  lookupEnvLevel = flip (Vec.!)

instance HasEnvLevel () where
  type EnvLevelOf () = Identity
  lookupEnvLevel () = runIdentity

class (Functor (EnvOf v)) => HasEnv v where
  type EnvOf v :: * -> *
  lookupEnv :: v -> EnvOf v a -> a

instance HasEnv Void where
  type EnvOf Void = Const ()
  lookupEnv = absurd

instance HasEnv Id.EVar where
  type EnvOf Id.EVar = Map.Map Id.EVar
  lookupEnv = lookupMap

instance (HasEnvLevel i, HasEnv v) => HasEnv (Scope b i v) where
  type EnvOf (Scope b i v) = Pair (EnvLevelOf i) (EnvOf v)
  lookupEnv i (Pair env_j env_v) = case i of
    Bound j _ -> lookupEnvLevel j env_j
    Free  v   -> lookupEnv v env_v

class BaseName b v where
  baseName :: v -> b

type BaseEVar ev = (BaseName Id.EVar ev)

type BaseTVar tv = (BaseName Id.TVar tv)

baseEVar :: BaseEVar ev => ev -> Id.EVar
baseEVar = baseName

baseTVar :: BaseTVar tv => tv -> Id.TVar
baseTVar = baseName

-- TODO: Remove this when it's no longer necessary.
instance BaseName Id.EVar Id.EVar where
  baseName = id

instance BaseName b Void where
  baseName = absurd

instance (BaseName b v) => BaseName b (Scope b i v) where
  baseName = \case
    Bound _ (Forget b) -> b
    Free  v            -> baseName v

makePrisms ''Scope

instance Bifunctor (Scope b) where
  bimap f g = \case
    Bound i b -> Bound (f i) b
    Free  x   -> Free  (g x)
