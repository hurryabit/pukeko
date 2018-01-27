{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
-- | Type for encoding DeBruijn style scoping.
module Pukeko.AST.Scope
  ( Scope (..)
  , EScope
  , EFinScope
  , TScope
  , TOneScope
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
  , finRenamer
  , voidEnv
  , extendEnv
  , HasEnvLevel (..)
  , HasEnv (..)
  , BaseEVar
  , BaseTVar
  , baseEVar
  , baseTVar
  , baseTVarIx
  )
  where

import Pukeko.Prelude

import           Data.Forget
import qualified Data.Map          as Map
import qualified Data.Vector.Sized as Vec

import qualified Pukeko.AST.Identifier as Id

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

type TOneScope = TScope ()

type TFinScope n = TScope (Finite n)

scope :: (v -> a) -> (i -> a) -> Scope b i v -> a
scope f g = scope' f (const . g)

scope' :: (v -> a) -> (i -> b -> a) -> Scope b i v -> a
scope' f g = \case
  Free  x            -> f x
  Bound i (Forget b) -> g i b

mkBound :: i -> b -> Scope b i v
mkBound i b = Bound i (Forget b)

strengthenEither :: Scope b i v -> Either (i, b) v
strengthenEither = scope' Right (\i b -> Left (i, b))

strengthen :: Scope b i v -> Maybe v
strengthen = either (const Nothing) Just . strengthenEither

strengthenWith :: (forall j. i -> j) -> Scope b i v -> v
strengthenWith f = either (f . fst) id . strengthenEither

unsafeStrengthen :: (HasCallStack, Show b) => Scope b i v -> v
unsafeStrengthen = either (bugWith "unsafeStrengthen" . snd) id . strengthenEither

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

finRenamer :: Ord b => Vector n b -> Map b (Scope b (Finite n) tv)
finRenamer = ifoldMap (\i b -> Map.singleton b (mkBound i b))

lookupMap :: (Ord i, Pretty i) => i -> Map i a -> a
lookupMap i = Map.findWithDefault (bugWith "lookup failed" (pretty i)) i

data Pair f g a = Pair (f a) (g a)
  deriving (Functor)

voidEnv :: EnvOf Void a
voidEnv = Const ()

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
  type EnvLevelOf Id.EVar = Map Id.EVar
  lookupEnvLevel = lookupMap

instance HasEnvLevel (Finite n) where
  type EnvLevelOf (Finite n) = Vector n
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
  baseNameIx :: v -> (b, Int)

baseName :: BaseName b v => v -> b
baseName = fst . baseNameIx

type BaseEVar ev = (BaseName Id.EVar ev)

type BaseTVar tv = (BaseName Id.TVar tv)

baseEVar :: BaseEVar ev => ev -> Id.EVar
baseEVar = baseName

baseTVarIx :: BaseTVar tv => tv -> (Id.TVar, Int)
baseTVarIx = baseNameIx

baseTVar :: BaseTVar tv => tv -> Id.TVar
baseTVar = baseName

instance BaseName b Void where
  baseNameIx = absurd

instance (BaseName b v) => BaseName b (Scope b i v) where
  baseNameIx = \case
    Bound _ (Forget b) -> (b, 0)
    Free  v            -> second succ (baseNameIx v)

instance Bifunctor (Scope b) where
  bimap f g = \case
    Bound i b -> Bound (f i) b
    Free  x   -> Free  (g x)
