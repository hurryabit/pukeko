{-# LANGUAGE MultiParamTypeClasses #-}
module CoreLang.Language.Term
  ( Term (..)
  , Subst
  , substVar
  , mkSubst
  , assign
  , exclude
  , TermCollection (..)
  , module Data.Monoid
  )
  where

import Data.Map (Map)
import Data.Monoid
import Data.Set (Set)

import qualified Data.Map as Map


class Ord (Var t) => Term t where
  data Var t
  supply   :: [Var t]
  promote  :: Var t -> t
  freeVars :: t -> Set (Var t)
  subst    :: Subst t -> t -> t

newtype Subst t = MkSubst { unSubst :: Map (Var t) t }

substVar :: Term t => Subst t -> Var t -> t
substVar phi v = Map.findWithDefault (promote v) v (unSubst phi)

mkSubst :: Map (Var t) t -> Subst t
mkSubst = MkSubst

assign :: Var t -> t -> Subst t
assign v t = MkSubst $ Map.singleton v t

exclude :: Term t => Subst t -> Set (Var t) -> Subst t
exclude phi vs = MkSubst $ Map.difference (unSubst phi) (Map.fromSet promote vs)

instance Term t => Monoid (Subst t) where
  mempty = MkSubst Map.empty
  phi `mappend` psi = MkSubst $ Map.union (Map.map (subst phi) (unSubst psi)) (unSubst phi)

instance Term t => TermCollection t t where
  freeVars' = freeVars
  subst' = subst

instance (Foldable f, Functor f, TermCollection t c) => TermCollection t (f c) where
  freeVars' = foldMap freeVars'
  subst' = fmap . subst'

class Term t => TermCollection t c where
  freeVars' :: c -> Set (Var t)
  subst'    :: Subst t -> c -> c
