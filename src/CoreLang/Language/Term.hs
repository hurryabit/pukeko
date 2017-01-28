{-# LANGUAGE TypeFamilyDependencies #-}
module CoreLang.Language.Term
  ( TermVar
  , Term (..)
  , Subst
  , substVar
  , mkSubst
  , assign
  , exclude
  , TermLike (..)
  , module Data.Monoid
  )
  where

import Data.Map (Map)
import Data.Monoid
import Data.Set (Set)

import qualified Data.Map as Map

type family TermVar t = v | v -> t

class Ord (TermVar t) => Term t where
  promoteVar :: TermVar t -> t
  freeVars   :: t -> Set (TermVar t)
  subst      :: Subst t -> t -> t

newtype Subst t = MkSubst { unSubst :: Map (TermVar t) t }

substVar :: Term t => Subst t -> TermVar t -> t
substVar phi v = Map.findWithDefault (promoteVar v) v (unSubst phi)

mkSubst :: Map (TermVar t) t -> Subst t
mkSubst = MkSubst

assign :: TermVar t -> t -> Subst t
assign v t = MkSubst $ Map.singleton v t

exclude :: Term t => Subst t -> Set (TermVar t) -> Subst t
exclude phi vs = MkSubst $ Map.difference (unSubst phi) (Map.fromSet promoteVar vs)

instance Term t => Monoid (Subst t) where
  mempty = MkSubst Map.empty
  phi `mappend` psi = MkSubst $ Map.union (Map.map (subst phi) (unSubst psi)) (unSubst phi)


class Term (BaseTerm ts) => TermLike ts where
  type BaseTerm ts
  freeVars' :: ts -> Set (TermVar (BaseTerm ts))
  subst'    :: Subst (BaseTerm ts) -> ts -> ts
