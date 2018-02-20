module Pukeko.FrontEnd.Gamma
  ( EffGamma
  , EffXGamma
  , runGamma
  , withinEScope
  , withinEScope'
  , withinEScope1
  , withQVars
  , withinXScope
  , lookupEVar
  , lookupTVar
  , lookupQual
  ) where

import Pukeko.Prelude

import qualified Data.Map.Extended as Map
import qualified Data.Vector as Vec

import           Pukeko.AST.Name
import           Pukeko.AST.SystemF
import           Pukeko.AST.Type

data Gamma tf ef ev = Gamma
  { _tenv :: EnvOf ev (ef NameTVar)
  , _kenv :: Map NameTVar (tf ev)
  }
makeLenses ''Gamma

type EffXGamma tf ef ev effs = Eff (Reader (Gamma tf ef ev) : effs)

type EffGamma ev effs = EffXGamma (Const (Set (Name Clss))) GenType ev effs

runGamma :: EffXGamma tf ef Void effs a -> Eff effs a
runGamma = runReader (Gamma voidEnv mempty)

withinEScope :: forall i tf ef ev effs a x. (HasEnvLevel i, HasEnv ev, Functor tf) =>
  (x -> ef NameTVar) -> EnvLevelOf i x -> EffXGamma tf ef (EScope i ev) effs a ->
  EffXGamma tf ef ev effs a
withinEScope f ts = localX (extendEnv @i @ev (fmap f ts)) (fmap (fmap weakenScope))

withinEScope' :: forall tf ef ev t effs a x. (HasEnv ev, Functor tf, Foldable t) =>
  (x -> ef NameTVar) -> t x -> EffXGamma tf ef (EScope Int ev) effs a ->
  EffXGamma tf ef ev effs a
withinEScope' f = withinEScope f . Vec.fromList . toList

withinEScope1 :: forall tf ef ev effs a x. (HasEnv ev, Functor tf) =>
  (x -> ef NameTVar) -> x -> EffXGamma tf ef (EScope () ev) effs a ->
  EffXGamma tf ef ev effs a
withinEScope1 f = withinEScope f . Identity

withinTScope :: forall tf ef ev effs a. (HasEnv ev, Functor ef) =>
  Map NameTVar (tf ev) ->
  EffXGamma tf ef ev effs a ->
  EffXGamma tf ef ev effs a
withinTScope kenv1 = locally (kenv @tf @ef @ev) (kenv1 <>)

withQVars :: (HasEnv ev, Foldable t) =>
  t QVar -> EffGamma ev effs a -> EffGamma ev effs a
withQVars = withinTScope . Map.fromList . map (\(MkQVar q v) -> (v, Const q)) . toList

withinXScope :: forall i tf ef ev effs a.
  (HasEnvLevel i, HasEnv ev, Functor tf, Functor ef) =>
  Map NameTVar (tf (EScope i ev)) -> EnvLevelOf i (ef NameTVar) ->
  EffXGamma tf ef (EScope i ev) effs a -> EffXGamma tf ef ev effs a
withinXScope qs ts = localX
  (extendEnv @i @ev ts)
  ((qs <>) . fmap (fmap weakenScope))

lookupEVar ::
  forall tf ef ev effs. (HasEnv ev) => ev -> EffXGamma tf ef ev effs (ef NameTVar)
lookupEVar = views (tenv @tf @ef @ev) . lookupEnv

lookupTVar ::
  forall tf ef ev effs. NameTVar -> EffXGamma tf ef ev effs (tf ev)
lookupTVar = views (kenv @tf @ef @ev) . (flip (Map.!))

lookupQual :: NameTVar -> EffGamma ev effs (Set (Name Clss))
lookupQual = fmap getConst . lookupTVar

localX ::
  (EnvOf ev1 (ef1 NameTVar) -> EnvOf ev2 (ef2 NameTVar)) ->
  (Map NameTVar (tf1 ev1) -> Map NameTVar (tf2 ev2)) ->
  EffXGamma tf2 ef2 ev2 effs a -> EffXGamma tf1 ef1 ev1 effs a
localX f g = local' upd
  where upd = (\(Gamma ev_env tv_env) -> Gamma (f ev_env) (g tv_env))
