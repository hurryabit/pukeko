{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
-- | Type for encoding DeBruijn style scoping.
module Pukeko.AST.Scope
  ( Scope (..)
  , EScope
  , TScope
  , scope
  , scope'
  , _Bound
  , _Free
  , mkBound
  , strengthenScope
  , strengthenScope0
  , unsafeStrengthenScope
  , weakenScope
  , instantiate
  , instantiateN
  , instantiate'
  , instantiateN'
  , abstract1
  , dist
  , (>>>=)
  , finRenamer
  , voidEnv
  , extendEnv
  , extendEnv'
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

import           Control.Lens.Indexed
import           Data.Forget
import qualified Data.Map          as Map
import qualified Data.Vector       as Vec
import           Data.Aeson.TH

import qualified Pukeko.AST.Identifier as Id

-- NOTE: The order of the constructors is chosen such that the derived @Ord@
-- instance sorts the corresponding de Bruijn indices in decreasing order, which
-- is good for lambda lifting.
data Scope b i v
  = Free v
  | Bound i (Forget b)
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

type EScope = Scope Id.EVar

type TScope = Scope Id.TVar

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

strengthenScope :: Scope b i v -> Maybe v
strengthenScope = either (const Nothing) Just . strengthenScopeEither

strengthenScope0 :: (HasCallStack, Show b) => Scope b Int v -> v
strengthenScope0 = either (bugWith "strengthenScope0") id . strengthenScopeEither

unsafeStrengthenScope :: (HasCallStack, Show b) => Scope b i v -> v
unsafeStrengthenScope =
  either (bugWith "unsafeStrengthenScope" . snd) id . strengthenScopeEither

weakenScope :: v -> Scope b i v
weakenScope = Free

instantiate :: Monad f => (i -> f v) -> f (Scope b i v) -> f v
instantiate f e = e >>= scope pure f

instantiate' :: Monad f => (i -> f v) -> f (Scope b i Void) -> f v
instantiate' f e = e >>= scope absurd f

instantiateN :: (HasCallStack, Monad f, Foldable t) =>
  t (f v) -> f (Scope b Int v) -> f v
instantiateN xsL = instantiate lk
  where xsV = Vec.fromList (toList xsL)
        lk i = maybe (bugWith "instantiateN" i) id (xsV Vec.!? i)

instantiateN' :: (HasCallStack, Monad f, Foldable t) =>
  t (f v) -> f (Scope b Int Void) -> f v
instantiateN' xsL = instantiate' lk
  where xsV = Vec.fromList (toList xsL)
        lk i = maybe (bugWith "instantiateN" i) id (xsV Vec.!? i)

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

finRenamer :: (Ord b, FoldableWithIndex Int t) => t b -> Map b (Scope b Int tv)
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

extendEnv' ::
  forall i v t a b.
  (HasEnvLevel i, EnvLevelOf i ~ Vector, HasEnv v, Foldable t) =>
  t a ->
  EnvOf v a ->
  EnvOf (Scope b i v) a
extendEnv' = extendEnv @i @v . Vec.fromList . toList

class (Functor (EnvLevelOf i)) => HasEnvLevel i where
  type EnvLevelOf i :: * -> *
  lookupEnvLevel :: i -> EnvLevelOf i a -> a

instance HasEnvLevel Id.EVar where
  type EnvLevelOf Id.EVar = Map Id.EVar
  lookupEnvLevel = lookupMap

instance HasEnvLevel Int where
  type EnvLevelOf Int = Vec.Vector
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

deriveToJSON defaultOptions ''Scope
