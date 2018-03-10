{-# LANGUAGE AllowAmbiguousTypes #-}
-- TODO: Fix naming.
module Pukeko.FrontEnd.Inferencer.Gamma
  ( Gamma
  , CanGamma
  , runGamma
  , withinEScope
  , withinEScope1
  , enterLevel
  , introTVar
  , introTVars
  , withinContext1
  , withinContext
  , getTLevel
  , lookupEVar
  , lookupTVar
  ) where

import Pukeko.Prelude

import qualified Data.Map.Extended as Map
import qualified Data.Set          as Set

import           Pukeko.AST.Name
import           Pukeko.FrontEnd.Inferencer.UType

data Gamma s = Gamma
  { _tenv  :: Map TmVar (UType s)
  , _level :: Level
  , _qenv  :: Map TyVar (Set Class)
  }
makeLenses ''Gamma

type CanGamma s effs = Member (Reader (Gamma s)) effs

runGamma :: Eff (Reader (Gamma s) : effs) a -> Eff effs a
runGamma = runReader (Gamma Map.empty topLevel Map.empty)

withinEScope :: CanGamma s effs => Map TmVar (UType s) -> Eff effs a -> Eff effs a
withinEScope ts = locally tenv (ts `Map.union`)

withinEScope1 :: CanGamma s effs => TmVar -> UType s -> Eff effs a -> Eff effs a
withinEScope1 x t = withinEScope (Map.singleton x t)

enterLevel :: forall s effs a. CanGamma s effs => Eff effs a -> Eff effs a
enterLevel = locally (level @s) succ

introTVar :: forall s effs a. CanGamma s effs =>
  TyVar -> Eff effs a -> Eff effs a
introTVar v = locally (qenv @s) (Map.insertWith impossible v Set.empty)

introTVars :: forall s t effs a. (CanGamma s effs, Foldable t) =>
  t TyVar -> Eff effs a -> Eff effs a
introTVars vs act = foldr (introTVar @s) act vs

withinContext1 :: forall s effs a. CanGamma s effs =>
  UTypeCstr s -> Eff effs a -> Eff effs a
withinContext1 (clss, UTVar v) =
  locally (qenv @s) (Map.alter (maybe impossible (Just . Set.insert clss)) v)
withinContext1 _ = impossible

withinContext :: forall s effs a. CanGamma s effs =>
  [UTypeCstr s] -> Eff effs a -> Eff effs a
withinContext cstrs act = foldr withinContext1 act cstrs

getTLevel :: forall s effs. CanGamma s effs => Eff effs Level
getTLevel = view (level @s)

lookupEVar :: CanGamma s effs => TmVar -> Eff effs (UType s)
lookupEVar x = views tenv (Map.! x)

lookupTVar :: forall s effs. CanGamma s effs => TyVar -> Eff effs (Set Class)
lookupTVar v = views (qenv @s) (Map.! v)
