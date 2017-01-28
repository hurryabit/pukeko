module CoreLang.Language.Subst
  ( Subst
  , runSubst
  , fromMap
  , assign
  , exclude
  , extend
  , Apply (..)
  , unify
  , unifyMany
  )
  where

import Control.Monad.Except
import Data.Map (Map)
import Data.Monoid
import Data.Set (Set)
import Text.Printf

import qualified Data.Map as Map
import qualified Data.Set as Set

import CoreLang.Language.Type

newtype Subst = MkSubst { unSubst :: Map Var Expr }

runSubst :: Subst -> Var -> Expr
runSubst phi v = Map.findWithDefault (Var v) v (unSubst phi)

fromMap :: Map Var Expr -> Subst
fromMap = MkSubst

assign :: Var -> Expr -> Subst
assign v t = MkSubst $ Map.singleton v t

exclude :: Subst -> Set Var -> Subst
exclude phi vs = MkSubst $ Map.difference (unSubst phi) (Map.fromSet Var vs)


instance Monoid Subst where
  mempty = MkSubst Map.empty
  phi `mappend` psi = MkSubst $ Map.union (Map.map (subst phi) (unSubst psi)) (unSubst phi)


class Apply a where
  subst :: Subst -> a -> a

instance (Functor t, Apply a) => Apply (t a) where
  subst phi = fmap (subst phi)

instance Apply Expr where
  subst phi t =
    case t of
      Var  v    -> runSubst phi v
      Cons c ts -> Cons c (subst phi ts)

instance Apply Scheme where
  subst phi (scheme@MkScheme { _schematicVars, _expr }) =
    scheme { _expr = subst (phi `exclude` _schematicVars) _expr }


-- TODO: Inline extend into unifyVar
extend :: MonadError String m => Subst -> Var -> Expr -> m Subst
extend phi v t =
  case t of
    Var u
      | u == v                    -> return phi
    _
      | Set.member v (freeVars t) -> throwError ("cyclic type variable " ++ show v)
      | otherwise                 -> return (assign v t <> phi)

unifyVar :: MonadError String m => Subst -> Var -> Expr -> m Subst
unifyVar phi v t =
  case runSubst phi v of
    Var u
      | u == v -> extend phi v phi_t
    phi_v      -> unify phi phi_v phi_t
    where
      phi_t = subst phi t

unify :: MonadError String m => Subst -> Expr -> Expr -> m Subst
unify phi t1 t2 =
  case (t1, t2) of
    (Var v1     , _          ) -> unifyVar phi v1 t2
    (Cons _ _   , Var v2     ) -> unifyVar phi v2 t1
    (Cons c1 ts1, Cons c2 ts2)
      | c1 /= c2               -> throwError (printf "mismatching types %s and %s" (show t1) (show t2))
      | otherwise              -> unifyMany phi (zip ts1 ts2)

unifyMany :: MonadError String m => Subst -> [(Expr, Expr)] -> m Subst
unifyMany = foldM (uncurry . unify)
