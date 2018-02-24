{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.FrontEnd.Inferencer.UType
  ( Level
  , UVarId
  , UType (..)
  , UVar (..)
  , topLevel
  , uvarIdName
  , uvarIds
  , mkUTUni
  , unUTUni
  , unUTUni1
  , (~>)
  , appTCon
  , unUTApp
  , open
  , open1
  , substUType
  , prettyUType
  )
  where

import Pukeko.Prelude
import Pukeko.Pretty

import qualified Bound as B
import qualified Bound.Name as B
import           Control.Monad.ST
import           Data.STRef
import qualified Data.Map.Extended as Map

import           Pukeko.AST.Name
import           Pukeko.AST.Type       hiding ((~>), (*~>))

infixr 1 ~>

newtype Level = Level Int
  deriving (Eq, Ord, Enum)

newtype UVarId = UVarId Int

data UVar s
  = UFree UVarId Level
  | ULink (UType s)

data UType s
  = UTVar (Name TVar)
  | UTAtm TypeAtom
  | UTApp (UType s) (UType s)
  | UTUni (NonEmpty QVar) (UType s)
  | UVar (STRef s (UVar s))

pattern UTFun :: UType s -> UType s -> UType s
pattern UTFun tx ty = UTApp (UTApp (UTAtm TAArr) tx) ty

topLevel :: Level
topLevel = Level 0

uvarIds :: [UVarId]
uvarIds = map UVarId [1 ..]

uvarIdName :: UVarId -> Tagged TVar String
uvarIdName (UVarId n) = Tagged ('_':show n)

mkUTUni :: [QVar] -> UType s -> UType s
mkUTUni xs0 t0 = case xs0 of
  [] -> t0
  x:xs -> UTUni (x :| xs) t0

-- TODO: Follow links.
unUTUni1 :: UType s -> ([QVar], UType s)
unUTUni1 = \case
  UTUni qvs t1 -> (toList qvs, t1)
  t0 -> ([], t0)

unUTUni :: UType s -> ([[QVar]], UType s)
unUTUni = go []
  where
    go qvss = \case
      UTUni qvs t -> go (toList qvs:qvss) t
      t -> (reverse qvss, t)

(~>) :: UType s -> UType s -> UType s
(~>) = UTFun

appTCon :: Name TCon -> [UType s] -> UType s
appTCon = foldl UTApp . UTAtm . TACon

unUTApp :: UType s -> ST s (UType s, [UType s])
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

open :: Type -> UType s
open = open1 . fmap UTVar

open1 :: GenType (UType s) -> UType s
open1 = \case
  TVar t -> t
  TAtm a -> UTAtm a
  TApp tf tp -> UTApp (open1 tf) (open1 tp)
  TUni xs tq -> UTUni xs (open1 (B.instantiate (TVar . UTVar . B.name) tq))

substUType :: Map (Name TVar) (UType s) -> UType s -> ST s (UType s)
substUType env = go
  where
    go t0 = case t0 of
      UTVar x -> pure (env Map.! x)
      UTAtm{} -> pure t0
      UTApp tf tp -> UTApp <$> go tf <*> go tp
      UTUni{} -> impossible  -- we have only rank-1 types
      UVar uref ->
        readSTRef uref >>= \case
          UFree{}  -> pure t0
          ULink t1 -> go  t1

-- * Pretty printing
instance Pretty UVarId where
  pretty = pretty . uvarIdName

prettyUVar :: Int -> UVar s -> ST s (Doc ann)
prettyUVar prec = \case
  UFree v _ -> pure (pretty v)
  ULink t   -> prettyUType prec t

prettyUType :: Int -> UType s -> ST s (Doc ann)
prettyUType prec = \case
  UTVar v -> pure (pretty v)
  UTAtm a -> pure (pretty a)
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
