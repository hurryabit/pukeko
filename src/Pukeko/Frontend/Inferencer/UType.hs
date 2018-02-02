{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.FrontEnd.Inferencer.UType
  ( UType (..)
  , UVar (..)
  , mkUTUni
  , unUTUni
  , unUTUni1
  , (~>)
  , (*~>)
  , appN
  , appTCon
  , unUTApp
  , open
  , open1
  , subst
  , prettyUVar
  , prettyUType
  )
  where

import Pukeko.Prelude

import           Control.Monad.ST
import           Data.Coerce        (coerce)
import           Data.STRef
import qualified Data.Map           as Map
import qualified Data.Vector        as Vec

import           Pukeko.Pretty
import           Pukeko.AST.Type       (Type (..), QVar (..), prettyTUni)
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Scope

infixr 1 ~>, *~>

data UVar s tv
  = UFree Id.TVar Int
    -- NOTE: The @Int@ is the level of this unification variable.
  | ULink (UType s tv)

data UType s tv
  = UTVar Id.TVar
  | UTArr
  | UTCon Id.TCon
  | UTApp (UType s tv) (UType s tv)
  | UTUni (NonEmpty QVar) (UType s tv)
  | UVar (STRef s (UVar s tv))

pattern UTFun :: UType s tv -> UType s tv -> UType s tv
pattern UTFun tx ty = UTApp (UTApp UTArr tx) ty

mkUTUni :: [QVar] -> UType s tv -> UType s tv
mkUTUni xs0 t0 = case xs0 of
  [] -> t0
  x:xs -> UTUni (x :| xs) t0

-- FIXME: Follow links.
unUTUni1 :: UType s tv -> ([QVar], UType s tv)
unUTUni1 = \case
  UTUni qvs t1 -> (toList qvs, t1)
  t0 -> ([], t0)

unUTUni :: UType s tv -> ([[QVar]], UType s tv)
unUTUni = go []
  where
    go qvss = \case
      UTUni qvs t -> go (toList qvs:qvss) t
      t -> (reverse qvss, t)

(~>) :: UType s tv -> UType s tv -> UType s tv
(~>) = UTFun

(*~>) :: Foldable t => t (UType s tv) -> UType s tv -> UType s tv
t_args *~> t_res = foldr (~>) t_res t_args

appN :: UType s tv -> [UType s tv] -> UType s tv
appN = foldl UTApp

appTCon :: Id.TCon -> [UType s tv] -> UType s tv
appTCon = appN . UTCon

unUTApp :: UType s tv -> ST s (UType s tv, [UType s tv])
unUTApp = go []
  where
    go tps = \case
      UTApp tf tp -> go (tp:tps) tf
      t0@(UVar uref) -> do
        uvar <- readSTRef uref
        case uvar of
          ULink t1 -> go tps t1
          UFree{}  -> pure (t0, tps)
      t0           -> pure (t0, tps)

open :: (BaseTVar tv) => Type tv -> UType s tv
open = open1 . fmap (UTVar . baseTVar)

open1 :: Type (UType s tv) -> UType s tv
open1 = \case
  TVar t -> t
  TArr -> UTArr
  TCon c -> UTCon c
  TApp tf tp -> UTApp (open1 tf) (open1 tp)
  TUni xs tq -> UTUni xs (open1 (fmap (scope id utvar) tq))
    where
      utvar = UTVar . _qvar2tvar . (Vec.fromList (toList xs) Vec.!)

subst :: Map Id.TVar (UType s tv) -> UType s tv -> ST s (UType s tv)
subst env = go
  where
    go = \case
      UTVar x -> do
        let e = bugWith "unknown type variable in instantiation" x
        pure (Map.findWithDefault e x env)
      t@UTArr     -> pure t
      t@(UTCon _) -> pure t
      UTApp tf tp -> UTApp <$> go tf <*> go tp
      t0@(UTUni _qvs _t1) -> do
        p0 <- prettyUType 0 t0
        bug ("quantification during instantiation: " ++ render p0)
        -- let vs = setOf (traverse . qvar2tvar) qvs
        -- subst (env `Map.difference` Map.fromSet (const ()) vs) t1
      t0@(UVar uref) -> do
        uvar <- readSTRef uref
        case uvar of
          UFree _ _ -> pure t0
          ULink t1  -> go  t1

-- * Pretty printing
prettyUVar :: Int -> UVar s tv -> ST s (Doc ann)
prettyUVar prec = \case
  UFree v _ -> pure (pretty v)
  ULink t   -> prettyUType prec t

prettyUType :: Int -> UType s tv -> ST s (Doc ann)
prettyUType prec = \case
  UTVar v -> pure (pretty v)
  UTArr   -> pure "(->)"
  UTCon c -> pure (pretty c)
  UTFun tx ty -> do
    px <- prettyUType 2 tx
    py <- prettyUType 1 ty
    pure $ maybeParens (prec > 1) $ px <+> "->" <+> py
  UTApp tf tx -> do
    pf <- prettyUType 2 tf
    px <- prettyUType 3 tx
    pure $ maybeParens (prec > 2) $ pf <+> px
  UTUni qvs tq -> prettyTUni prec qvs <$> prettyUType 0 tq
  UVar uref -> do
    uvar <- readSTRef uref
    prettyUVar prec uvar

instance Functor (UType s) where
  fmap _ = coerce
