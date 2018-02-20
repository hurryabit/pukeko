module Pukeko.FrontEnd.Inferencer.Gamma
  ( Gamma
  , EffGamma
  , runGamma
  , withinEScope
  , withinEScope1
  , withinTScope
  , withQVars
  , getTLevel
  , lookupEVar
  , lookupTVar
  ) where

import Pukeko.Prelude

import           Control.Lens ((<>~))
import           Data.Coerce (coerce)
import qualified Data.Map.Extended as Map

import           Pukeko.AST.Name
import           Pukeko.AST.Scope
import           Pukeko.AST.Type
import           Pukeko.FrontEnd.Inferencer.UType

data Gamma s ev = Gamma
  { _tenv  :: EnvOf ev (UType s)
  , _level :: Level
  , _qenv  :: Map (Name TVar) (Set (Name Clss))
  }
makeLenses ''Gamma

type EffGamma s ev effs = Eff (Reader (Gamma s ev) : effs)

runGamma :: EffGamma s Void effs a -> Eff effs a
runGamma = runReader (Gamma voidEnv topLevel mempty)

withinEScope :: forall i s ev effs a. (HasEnvLevel i, HasEnv ev) =>
  EnvLevelOf i (UType s) -> EffGamma s (EScope i ev) effs a -> EffGamma s ev effs a
withinEScope ts = local' (tenv %~ extendEnv @i @ev (fmap coerce ts))

withinEScope1 :: forall s ev effs a. (HasEnv ev) =>
  UType s -> EffGamma s (EScope () ev) effs a -> EffGamma s ev effs a
withinEScope1 = withinEScope . Identity

withinTScope :: forall s ev effs a. EffGamma s ev effs a -> EffGamma s ev effs a
withinTScope = locally (level @s @ev) succ

-- TODO: It's not entirely clear to me if not changing the level is the right
-- thing to do for future uses, particularly existential types.
withQVars :: forall s ev t effs a. (Foldable t) =>
  t QVar -> EffGamma s ev effs a -> EffGamma s ev effs a
withQVars qvs =
  local (qenv @s @ev <>~ foldMap (\(MkQVar q v) -> Map.singleton v q) qvs)

getTLevel :: forall s ev effs. EffGamma s ev effs Level
getTLevel = view (level @s @ev)

lookupEVar :: forall s ev effs. (HasEnv ev) => ev -> EffGamma s ev effs (UType s)
lookupEVar x = views (tenv @s @ev) (lookupEnv x)

lookupTVar :: forall s ev effs. Name TVar -> EffGamma s ev effs (Set (Name Clss))
lookupTVar x = views (qenv @s @ev) (Map.! x)
