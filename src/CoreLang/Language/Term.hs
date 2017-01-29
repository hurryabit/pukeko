module CoreLang.Language.Term
  ( Term (..)
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


class Term (BaseTerm ts) => TermLike ts where
  type BaseTerm ts
  freeVars' :: ts -> Set (Var (BaseTerm ts))
  subst'    :: Subst (BaseTerm ts) -> ts -> ts
