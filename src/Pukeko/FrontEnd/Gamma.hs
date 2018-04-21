{-# LANGUAGE TemplateHaskell #-}
module Pukeko.FrontEnd.Gamma
  ( CanGamma
  , runGamma
  , introTmVar
  , introTmVars
  , introTyVar
  , introTyVars
  , introDxVar
  , introDxVars
  , introPar
  , introPars
  , lookupTmVar
  , lookupTmVarIx
  , lookupTyVar
  , lookupDxVar
  ) where

import Pukeko.Prelude

import qualified Data.Map.Extended as Map

import           Pukeko.AST.Expr (Par (..), TmBinder)
import           Pukeko.AST.Language
import           Pukeko.AST.Name
import           Pukeko.AST.Dict
import           Pukeko.AST.Type

data Gamma = Gamma
  { _tmVars :: Map TmVar (Type, Int)
  , _tyVars :: Map TyVar ()
  , _dxVars :: Map DxVar TypeCstr
  }
makeLenses ''Gamma

type CanGamma effs = Member (Reader Gamma) effs

runGamma :: Eff (Reader Gamma : effs) a -> Eff effs a
runGamma = runReader (Gamma Map.empty Map.empty Map.empty)

shadowing :: (HasCallStack, Show k) => String -> k -> a -> a -> a
shadowing s k _ _ = bugWith ("shadowing " ++ s) k

introTmVar :: (HasCallStack, CanGamma effs) => TmBinder Type -> Eff effs a -> Eff effs a
introTmVar (x, t) =
  locally tmVars
    (\yts -> Map.insertWithKey (shadowing "term variable") x (t, Map.size yts) yts)

introTmVars :: CanGamma effs => [TmBinder Type] -> Eff effs a -> Eff effs a
introTmVars xts act = foldr introTmVar act xts

introTyVar :: CanGamma effs => TyVar -> Eff effs a -> Eff effs a
introTyVar v = locally tyVars (Map.insertWith impossible v ())

introTyVars :: (CanGamma effs, Foldable t) => t TyVar -> Eff effs a -> Eff effs a
introTyVars (toList -> vs) =
  -- TODO: fold over 'introTyVar'
  -- we try to avoid shadowing everywhere
  locally tyVars (Map.unionWith impossible (Map.fromList (zip vs (repeat ()))))

introDxVar :: CanGamma effs => DxBinder Type -> Eff effs a -> Eff effs a
introDxVar (x, c) = locally dxVars (Map.insertWith impossible x c)

introDxVars :: CanGamma effs => [DxBinder Type] -> Eff effs a -> Eff effs a
introDxVars xcs = locally dxVars (Map.unionWith impossible (Map.fromList xcs))

introPar :: (CanGamma effs, TypeOf lg ~ Type) => Par lg -> Eff effs a -> Eff effs a
introPar = \case
  TmPar xt -> introTmVar xt
  TyPar v  -> introTyVar v
  CxPar xc -> introDxVar xc

introPars :: (CanGamma effs, TypeOf lg ~ Type) => [Par lg] -> Eff effs a -> Eff effs a
introPars pars act = foldr introPar act pars

lookupTmVarIx :: (HasCallStack, CanGamma effs) => TmVar -> Eff effs (Type, Int)
lookupTmVarIx x = views tmVars (Map.lookup x) >>= \case
  Nothing -> bugWith "unknown term variable" x
  Just t  -> pure t

lookupTmVar :: (HasCallStack, CanGamma effs) => TmVar -> Eff effs Type
lookupTmVar = fmap fst . lookupTmVarIx

lookupTyVar :: CanGamma effs => TyVar -> Eff effs ()
lookupTyVar = views tyVars . flip (Map.!)

lookupDxVar :: CanGamma effs => DxVar -> Eff effs TypeCstr
lookupDxVar = views dxVars . flip (Map.!)
