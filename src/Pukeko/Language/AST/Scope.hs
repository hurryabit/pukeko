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
  , strengthenWith
  , unsafeStrengthen
  , weaken
  , abstract1
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

import           Pukeko.Error      (bugWith)
import           Pukeko.Pretty
import qualified Pukeko.Language.Ident as Id

-- NOTE: The order of the constructors is chosen such that the derived @Ord@
-- instance sorts the corresponding de Bruijn indices in decreasing order, which
-- is good for lambda lifting.
data Scope b i v
  = Free v
  | Bound i (Forget b)
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

type EScope = Scope Id.EVar

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

strengthenEither :: Scope b i v -> Either (i, b) v
strengthenEither = \case
  Bound i (Forget b) -> Left  (i, b)
  Free  x            -> Right x

strengthen :: Scope b i v -> Maybe v
strengthen = either (const Nothing) Just . strengthenEither

strengthenWith :: (forall j. i -> j) -> Scope b i v -> v
strengthenWith f = either (f . fst) id . strengthenEither

-- TODO: Use @HasCallstack@.
unsafeStrengthen :: Show b => Scope b i v -> v
unsafeStrengthen =
  either (\(_, b) -> error ("cannot strengthen " ++ show b)) id . strengthenEither

weaken :: v -> Scope b i v
weaken = Free

abstract1 :: (j -> Maybe i) -> Scope b j v -> Scope b j (Scope b i v)
abstract1 f = \case
  Bound (f -> Just i) b -> Free (Bound i b)
  Bound j             b -> Bound j b
  Free  x               -> Free (Free x)

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

lookupMap :: (Ord i, Pretty i) => i -> Map.Map i a -> a
lookupMap i = Map.findWithDefault (bugWith "lookup failed" (pretty i)) i

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

instance BaseName b Void where
  baseName = absurd

instance (BaseName b v) => BaseName b (Scope b i v) where
  baseName = \case
    Bound _ (Forget b) -> b
    Free  v            -> baseName v

instance Bifunctor (Scope b) where
  bimap f g = \case
    Bound i b -> Bound (f i) b
    Free  x   -> Free  (g x)
