module Pukeko.FrontEnd.Gamma
  ( CanGamma
  , runGamma
  , withinEScope
  , withinEScope1
  , withinScope1
  , withinScope
  , lookupEVar
  , lookupEVarIx
  , lookupTVar
  ) where

import Pukeko.Prelude

import qualified Data.Map.Extended as Map
import qualified Data.Set          as Set

import           Pukeko.AST.Expr (Par (..), EVarBinder)
import           Pukeko.AST.Language
import           Pukeko.AST.Name
import           Pukeko.AST.Type

data Gamma = Gamma
  { _evars :: Map NameEVar (Type, Int)
  , _tvars :: Map NameTVar (Set NameClss)
  }
makeLenses ''Gamma

type CanGamma effs = Member (Reader Gamma) effs

runGamma :: Eff (Reader Gamma : effs) a -> Eff effs a
runGamma = runReader (Gamma Map.empty Map.empty)

withinEScope1 :: CanGamma effs => EVarBinder Type -> Eff effs a -> Eff effs a
withinEScope1 (x, t) =
  locally evars (\evs -> Map.insertWith impossible x (t, Map.size evs) evs)

withinEScope :: CanGamma effs => [EVarBinder Type] -> Eff effs a -> Eff effs a
withinEScope xts act = foldr withinEScope1 act xts

withinScope1 :: (CanGamma effs, TypeOf lg ~ Type) => Par lg -> Eff effs a -> Eff effs a
withinScope1 = \case
  TmPar x  -> withinEScope1 x
  TyPar v  -> locally tvars (Map.insertWith impossible v Set.empty)
  CxPar (clss, t)
    | TVar v <- t ->
      locally tvars (Map.alter (maybe impossible (Just . Set.insert clss)) v)
    | otherwise   -> impossible  -- we only allow constraints of the form @C a@

withinScope :: (CanGamma effs, TypeOf lg ~ Type) => [Par lg] -> Eff effs a -> Eff effs a
withinScope pars act = foldr withinScope1 act pars

lookupEVarIx :: CanGamma effs => NameEVar -> Eff effs (Type, Int)
lookupEVarIx x = views evars (Map.! x)

lookupEVar :: CanGamma effs => NameEVar -> Eff effs Type
lookupEVar = fmap fst . lookupEVarIx

lookupTVar :: CanGamma effs => NameTVar -> Eff effs (Set NameClss)
lookupTVar = views tvars . flip (Map.!)
