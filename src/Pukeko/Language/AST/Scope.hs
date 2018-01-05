{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}
-- | Type for encoding DeBruijn style scoping.
module Pukeko.Language.AST.Scope
  ( Scope (..)
  , FinScope
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
  )
  where

import           Control.Lens
import           Data.Finite       (Finite)
import           Data.Forget
import qualified Data.Map          as Map
import qualified Data.Vector.Sized as Vec

import           Pukeko.Error      (bug)
import           Pukeko.Pretty
import qualified Pukeko.Language.Ident as Id

data Scope i v
  = Bound i (Forget Id.EVar)
  | Free v
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

type FinScope n = Scope (Finite n)

scope :: (i -> a) -> (v -> a) -> Scope i v -> a
scope f g = \case
  Bound i _ -> f i
  Free  x   -> g x

mkBound :: i -> Id.EVar -> Scope i v
mkBound i x = Bound i (Forget x)

strengthen :: String -> Scope i v -> v
strengthen component = \case
  Bound _ (Forget x) -> bug component "cannot strengthen" (Just (show x))
  Free  x            -> x

weaken :: v -> Scope i v
weaken = Free

weaken1 :: Scope j v -> Scope j (Scope i v)
weaken1 = \case
  Bound j x -> Bound j x
  Free  x   -> Free  (Free x)

abstract1 :: (j -> Maybe i) -> Scope j v -> Scope j (Scope i v)
abstract1 f = \case
  Bound (f -> Just i) x -> Free (Bound i x)
  Bound j             x -> Bound j x
  Free  x               -> Free (Free x)

-- TODO: Find out where we use the inverse 'scope' without naming it like this.
-- It's probably in the de Bruijn indexer.
unscope :: Scope i Id.EVar -> Id.EVar
unscope = \case
  Bound _ (Forget x) -> x
  Free  x            -> x

lookupMap :: (Ord i, Pretty i) => i -> Map.Map i a -> a
lookupMap i = Map.findWithDefault (bug "scope" "lookup failed" (Just (prettyShow i))) i

data Pair f g a = Pair (f a) (g a)

extendEnv ::
  forall i v a.
  (IsVarLevel i, IsVar v) =>
  EnvLevelOf i a ->
  EnvOf v a ->
  EnvOf (Scope i v) a
extendEnv env_i env_v = Pair env_i env_v

-- TODO: Replace @Ord@ by @Eq@.
class Ord i => IsVarLevel i where
  type EnvLevelOf i :: * -> *
  lookupEnvLevel :: i -> EnvLevelOf i a -> a

instance IsVarLevel Id.EVar where
  type EnvLevelOf Id.EVar = Map.Map Id.EVar
  lookupEnvLevel = lookupMap

instance IsVarLevel (Finite n) where
  type EnvLevelOf (Finite n) = Vec.Vector n
  lookupEnvLevel = flip (Vec.!)

-- TODO: Replace @Ord@ by @Eq@.
class (Ord v, Pretty v) => IsVar v where
  type EnvOf v :: * -> *
  varName :: v -> Id.EVar
  lookupEnv :: v -> EnvOf v a -> a

instance IsVar Id.EVar where
  type EnvOf Id.EVar = Map.Map Id.EVar
  varName = id
  lookupEnv = lookupMap

instance (IsVarLevel i, IsVar v) => IsVar (Scope i v) where
  type EnvOf (Scope i v) = Pair (EnvLevelOf i) (EnvOf v)
  varName = \case
    Bound _ (Forget x) -> x
    Free  v            -> varName v
  lookupEnv i (Pair env_j env_v) = case i of
    Bound j _ -> lookupEnvLevel j env_j
    Free  v   -> lookupEnv v env_v

makePrisms ''Scope

instance Pretty v => Pretty (Scope i v) where
  pPrint = \case
    Bound _ (Forget x) -> pretty x
    Free v -> pretty v
