module Pukeko.FrontEnd.Gamma
  ( CanGamma
  , runGamma
  , introTmVar
  , introTmVars
  , introTyVar
  , introTyVars
  , introCstr
  , introCstrs
  , introPar
  , introPars
  , lookupTmVar
  , lookupTmVarIx
  , lookupTyVar
  ) where

import Pukeko.Prelude

import qualified Data.Map.Extended as Map
import qualified Data.Set          as Set

import           Pukeko.AST.Expr (Par (..), TmBinder)
import           Pukeko.AST.Language
import           Pukeko.AST.Name
import           Pukeko.AST.Type

data Gamma = Gamma
  { _tmVars :: Map TmVar (Type, Int)
  , _tyVars :: Map TyVar (Set Class)
  }
makeLenses ''Gamma

type CanGamma effs = Member (Reader Gamma) effs

runGamma :: Eff (Reader Gamma : effs) a -> Eff effs a
runGamma = runReader (Gamma Map.empty Map.empty)

introTmVar :: CanGamma effs => TmBinder Type -> Eff effs a -> Eff effs a
introTmVar (x, t) =
  locally tmVars (\yts -> Map.insertWith impossible x (t, Map.size yts) yts)

introTmVars :: CanGamma effs => [TmBinder Type] -> Eff effs a -> Eff effs a
introTmVars xts act = foldr introTmVar act xts

introTyVar :: CanGamma effs => TyVar -> Eff effs a -> Eff effs a
introTyVar v = locally tyVars (Map.insertWith impossible v Set.empty)

introTyVars :: (CanGamma effs, Foldable t) => t TyVar -> Eff effs a -> Eff effs a
introTyVars (toList -> vs) =
  -- TODO: fold over 'introTyVar'
  -- we try to avoid shadowing everywhere
  locally tyVars (Map.unionWith impossible (Map.fromList (zip vs (repeat Set.empty))))

introCstr :: CanGamma effs => TypeCstr -> Eff effs a -> Eff effs a
introCstr (clss, TVar v) =
  locally tyVars (Map.alter (maybe impossible (Just . Set.insert clss)) v)
introCstr _ = impossible  -- we only allow constraints of the form @C a@

introCstrs :: CanGamma effs => [TypeCstr] -> Eff effs a -> Eff effs a
introCstrs cstrs act = foldr introCstr act cstrs

introPar :: (CanGamma effs, TypeOf lg ~ Type) => Par lg -> Eff effs a -> Eff effs a
introPar = \case
  TmPar xt -> introTmVar xt
  TyPar v  -> introTyVar v
  CxPar cx -> introCstr  cx

introPars :: (CanGamma effs, TypeOf lg ~ Type) => [Par lg] -> Eff effs a -> Eff effs a
introPars pars act = foldr introPar act pars

lookupTmVarIx :: CanGamma effs => TmVar -> Eff effs (Type, Int)
lookupTmVarIx x = views tmVars (Map.! x)

lookupTmVar :: CanGamma effs => TmVar -> Eff effs Type
lookupTmVar = fmap fst . lookupTmVarIx

lookupTyVar :: CanGamma effs => TyVar -> Eff effs (Set Class)
lookupTyVar = views tyVars . flip (Map.!)
