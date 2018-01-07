{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
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
  , _Bound
  , _Free
  , mkBound
  , strengthen
  , weaken
  , weaken1
  , abstract1
  , unscope
  , extendEnv
  , IsVarLevel (..)
  , IsVar (..)
  , IsEVar
  , IsTVar
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
scope f g = \case
  Bound i _ -> f i
  Free  x   -> g x

mkBound :: i -> b -> Scope b i v
mkBound i b = Bound i (Forget b)

-- TODO: Use @HasCallstack@.
strengthen :: Show b => String -> Scope b i v -> v
strengthen component = \case
  Bound _ (Forget b) -> bug component "cannot strengthen" (Just (show b))
  Free  x            -> x

weaken :: v -> Scope b i v
weaken = Free

weaken1 :: Scope b j v -> Scope b j (Scope b i v)
weaken1 = \case
  Bound j b -> Bound j b
  Free  x   -> Free  (Free x)

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

lookupMap :: (Ord i, Pretty i) => i -> Map.Map i a -> a
lookupMap i = Map.findWithDefault (bug "scope" "lookup failed" (Just (prettyShow i))) i

data Pair f g a = Pair (f a) (g a)

extendEnv ::
  forall i v a.
  (IsVarLevel i, IsVar v) =>
  EnvLevelOf i a ->
  EnvOf v a ->
  EnvOf (Scope (BaseName v) i v) a
extendEnv env_i env_v = Pair env_i env_v

class (Functor (EnvLevelOf i)) => IsVarLevel i where
  type EnvLevelOf i :: * -> *
  lookupEnvLevel :: i -> EnvLevelOf i a -> a

instance IsVarLevel Id.EVar where
  type EnvLevelOf Id.EVar = Map.Map Id.EVar
  lookupEnvLevel = lookupMap

instance IsVarLevel (Finite n) where
  type EnvLevelOf (Finite n) = Vec.Vector n
  lookupEnvLevel = flip (Vec.!)

instance IsVarLevel () where
  type EnvLevelOf () = Identity
  lookupEnvLevel () = runIdentity

class IsVar v where
  type BaseName v :: *
  type EnvOf v :: * -> *
  baseName :: v -> BaseName v
  lookupEnv :: v -> EnvOf v a -> a

type IsEVar v = (IsVar v, BaseName v ~ Id.EVar)

type IsTVar v = (IsVar v, BaseName v ~ Id.TVar)

instance IsVar Id.EVar where
  type BaseName Id.EVar = Id.EVar
  type EnvOf Id.EVar = Map.Map Id.EVar
  baseName = id
  lookupEnv = lookupMap

instance (Pretty b, IsVarLevel i, IsVar v, BaseName v ~ b) => IsVar (Scope b i v) where
  type BaseName (Scope b i v) = b
  type EnvOf (Scope b i v) = Pair (EnvLevelOf i) (EnvOf v)
  baseName = \case
    Bound _ (Forget b) -> b
    Free  v            -> baseName v
  lookupEnv i (Pair env_j env_v) = case i of
    Bound j _ -> lookupEnvLevel j env_j
    Free  v   -> lookupEnv v env_v

instance IsVar Void where
  type BaseName Void = Id.TVar
  type EnvOf Void = Const ()
  baseName = absurd
  lookupEnv = absurd

makePrisms ''Scope
