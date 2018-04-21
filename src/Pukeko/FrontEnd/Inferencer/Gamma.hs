{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Pukeko.FrontEnd.Inferencer.Gamma
  ( Gamma
  , CanGamma
  , runGamma

  , introTmVar
  , introTmVars
  , lookupTmVar

  , enterLevel
  , getLevel

  , introTyVar
  , introTyVars
  , checkTyVar

  , enterContext
  , lookupContext
  ) where

import Pukeko.Prelude

import qualified Data.Map.Extended as Map

import Pukeko.AST.Dict (DxBinder)
import Pukeko.AST.Expr (TmBinder)
import Pukeko.AST.Name
import Pukeko.FrontEnd.Inferencer.UType

data Gamma s = Gamma
  { _tmVars :: Map TmVar (UType s)
  , _level  :: Level
  , _tyVars :: Map TyVar (Map Class DxVar)
  }
makeLenses ''Gamma

type CanGamma s effs = Member (Reader (Gamma s)) effs

runGamma :: Eff (Reader (Gamma s) : effs) a -> Eff effs a
runGamma = runReader (Gamma Map.empty topLevel Map.empty)

introTmVar :: CanGamma s effs => TmBinder (UType s) -> Eff effs a -> Eff effs a
introTmVar (x, t) = locally tmVars (Map.insertWith impossible x t)

introTmVars :: CanGamma s effs => [TmBinder (UType s)] -> Eff effs a -> Eff effs a
introTmVars = flip (foldr introTmVar)

lookupTmVar :: CanGamma s effs => TmVar -> Eff effs (UType s)
lookupTmVar x = views tmVars (Map.! x)

enterLevel :: forall s effs a. CanGamma s effs => Eff effs a -> Eff effs a
enterLevel = locally (level @s) succ

getLevel :: forall s effs. CanGamma s effs => Eff effs Level
getLevel = view (level @s)

introTyVar :: forall s effs a. CanGamma s effs => TyVar -> Eff effs a -> Eff effs a
introTyVar v = locally (tyVars @s) (Map.insertWith impossible v Map.empty)

introTyVars :: forall s effs a. CanGamma s effs => [TyVar] -> Eff effs a -> Eff effs a
introTyVars = flip (foldr (introTyVar @s))

checkTyVar :: forall s effs. CanGamma s effs => TyVar -> Eff effs ()
checkTyVar v = views (tyVars @s) (Map.member v) >>= assertM

enterContext1 :: forall s effs a. CanGamma s effs =>
  DxBinder (UType s) -> Eff effs a -> Eff effs a
enterContext1 (dvar, (clss, UTVar tvar)) = locally (tyVars @s) (Map.alter f tvar)
  where
    f = \case
      Nothing -> impossible
      Just dvars -> Just (Map.insertWith impossible clss dvar dvars)
enterContext1 _ = impossible

enterContext :: forall s effs a. CanGamma s effs =>
  [DxBinder (UType s)] -> Eff effs a -> Eff effs a
enterContext = flip (foldr enterContext1)

lookupContext :: forall s effs. CanGamma s effs =>
  TyVar-> Eff effs (Maybe (Map Class DxVar))
lookupContext v = views (tyVars @s) (Map.lookup v)
