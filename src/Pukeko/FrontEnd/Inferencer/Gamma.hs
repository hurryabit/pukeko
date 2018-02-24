{-# LANGUAGE AllowAmbiguousTypes #-}
module Pukeko.FrontEnd.Inferencer.Gamma
  ( Gamma
  , CanGamma
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

import qualified Data.Map.Extended as Map

import           Pukeko.AST.Name
import           Pukeko.AST.Type
import           Pukeko.FrontEnd.Inferencer.UType

data Gamma s = Gamma
  { _tenv  :: Map NameEVar (UType s)
  , _level :: Level
  , _qenv  :: Map (Name TVar) (Set (Name Clss))
  }
makeLenses ''Gamma

type CanGamma s effs = Member (Reader (Gamma s)) effs

runGamma :: Eff (Reader (Gamma s) : effs) a -> Eff effs a
runGamma = runReader (Gamma Map.empty topLevel Map.empty)

withinEScope :: CanGamma s effs => Map NameEVar (UType s) -> Eff effs a -> Eff effs a
withinEScope ts = locally tenv (ts `Map.union`)

withinEScope1 :: CanGamma s effs => NameEVar -> UType s -> Eff effs a -> Eff effs a
withinEScope1 x t = withinEScope (Map.singleton x t)

withinTScope :: forall s effs a. CanGamma s effs => Eff effs a -> Eff effs a
withinTScope = locally (level @s) succ

-- TODO: It's not entirely clear to me if not changing the level is the right
-- thing to do for future uses, particularly existential types.
withQVars :: forall s t effs a. (CanGamma s effs, Foldable t) =>
  t TVarBinder -> Eff effs a -> Eff effs a
withQVars qvs =
  let env = Map.fromList (toList qvs)
  in  locally (qenv @s) (env `Map.union`)

getTLevel :: forall s effs. CanGamma s effs => Eff effs Level
getTLevel = view (level @s)

lookupEVar :: CanGamma s effs => NameEVar -> Eff effs (UType s)
lookupEVar x = views tenv (Map.! x)

lookupTVar :: forall s effs. CanGamma s effs => NameTVar -> Eff effs (Set (Name Clss))
lookupTVar v = views (qenv @s) (Map.! v)
