{-# LANGUAGE ViewPatterns #-}
-- | Type for encoding DeBruijn style scoping.
module Pukeko.Language.AST.Scope
  ( Scope (..)
  , FinScope
  , _Bound
  , _Free
  , bound
  , strengthen
  , weaken
  , weaken1
  , abstract1
  , unscope
  , IsVar (..)
  )
  where

import           Control.Lens
import           Data.Finite       (Finite)
import           Data.Forget

import           Pukeko.Error      (bug)
import           Pukeko.Pretty
import qualified Pukeko.Language.Ident as Id

data Scope i v
  = Bound i (Forget Id.EVar)
  | Free v
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

type FinScope n = Scope (Finite n)

bound :: i -> Id.EVar -> Scope i v
bound i x = Bound i (Forget x)

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

class (Eq v, Ord v, Pretty v) => IsVar v where
  varName :: v -> Id.EVar
  isTotallyFree :: v -> Bool
  mkTotallyFree :: Id.EVar -> v

instance IsVar Id.EVar where
  varName = id
  isTotallyFree = const True
  mkTotallyFree = id

instance (Ord i, IsVar v) => IsVar (Scope i v) where
  varName = \case
    Bound _ (Forget x) -> x
    Free  v            -> varName v
  isTotallyFree = \case
    Bound _ _ -> False
    Free  v   -> isTotallyFree v
  mkTotallyFree = Free . mkTotallyFree

makePrisms ''Scope

instance Pretty v => Pretty (Scope i v) where
  pPrint = \case
    Bound _ (Forget x) -> pretty x
    Free v -> pretty v
