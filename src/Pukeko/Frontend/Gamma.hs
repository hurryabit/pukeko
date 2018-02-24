module Pukeko.FrontEnd.Gamma
  ( CanGamma
  , runGamma
  , withinEScope
  , withinEScope1
  , withQVars
  , withinTScope
  , lookupEVar
  , lookupEVarIx
  , lookupTVar
  ) where

import Pukeko.Prelude

import qualified Data.Map.Extended as Map

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

withinEScope1 :: CanGamma effs => NameEVar -> Type -> Eff effs a -> Eff effs a
withinEScope1 x t = locally evars (\evs -> Map.insert x (t, Map.size evs) evs)

withinEScope :: CanGamma effs => [(NameEVar, Type)] -> Eff effs a -> Eff effs a
withinEScope xts act = foldr (uncurry withinEScope1) act xts

withinTScope :: CanGamma effs => [(NameTVar, Set NameClss)] -> Eff effs a -> Eff effs a
withinTScope qs = locally tvars (Map.fromList qs <>)

withQVars :: (CanGamma effs, Foldable t) => t QVar -> Eff effs a -> Eff effs a
withQVars = withinTScope . map (\(MkQVar q v) -> (v, q)) . toList

lookupEVarIx :: CanGamma effs => NameEVar -> Eff effs (Type, Int)
lookupEVarIx x = views evars (Map.! x)

lookupEVar :: CanGamma effs => NameEVar -> Eff effs Type
lookupEVar = fmap fst . lookupEVarIx

lookupTVar :: CanGamma effs => NameTVar -> Eff effs (Set NameClss)
lookupTVar = views tvars . (flip (Map.!))
