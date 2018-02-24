module Pukeko.FrontEnd.Gamma
  ( EffGamma
  , EffXGamma
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

data Gamma ti ei = Gamma
  { _evars :: Map NameEVar (ei, Int)
  , _tvars :: Map NameTVar ti
  }
makeLenses ''Gamma

type EffXGamma ti ei effs = Eff (Reader (Gamma ti ei) : effs)

type EffGamma effs = EffXGamma (Set (Name Clss)) Type effs

runGamma :: EffXGamma ti ei effs a -> Eff effs a
runGamma = runReader (Gamma Map.empty Map.empty)

withinEScope :: [(NameEVar, ei)] -> EffXGamma ti ei effs a -> EffXGamma ti ei effs a
withinEScope ts act = foldr (uncurry withinEScope1) act ts

withinEScope1 :: forall ti ei effs a.
  NameEVar -> ei -> EffXGamma ti ei effs a -> EffXGamma ti ei effs a
withinEScope1 x t = locally (evars @ti @ei) (\evs -> Map.insert x (t, Map.size evs) evs)

withinTScope :: forall ti ei effs a.
  Map NameTVar ti -> EffXGamma ti ei effs a -> EffXGamma ti ei effs a
withinTScope qs = locally (tvars @ti @ei) (qs <>)

withQVars :: Foldable t => t QVar -> EffGamma effs a -> EffGamma effs a
withQVars = withinTScope . Map.fromList . map (\(MkQVar q v) -> (v, q)) . toList

lookupEVarIx :: forall ti ei effs. NameEVar -> EffXGamma ti ei effs (ei, Int)
lookupEVarIx x = views (evars @ti @ei) (Map.! x)

lookupEVar :: NameEVar -> EffXGamma ti ei effs ei
lookupEVar = fmap fst . lookupEVarIx

lookupTVar :: forall ti ei effs. NameTVar -> EffXGamma ti ei effs ti
lookupTVar = views (tvars @ti @ei) . (flip (Map.!))
