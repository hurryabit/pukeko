{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.FrontEnd.Inferencer.UType
  ( Level
  , UVarId
  , UVar (..)
  , UTypeCstr
  , UType (..)

  , pattern UTCon
  , _UTUni
  , _UTCtx

  , topLevel
  , uvarIdName
  , uvarIds
  , (~>)
  , unwindUTApp
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
import           Pukeko.AST.Type       hiding ((~>))

newtype Level = Level Int
  deriving (Eq, Ord, Enum)

newtype UVarId = UVarId Int

data UVar s
  = UFree UVarId Level
  | ULink (UType s)

type UTypeCstr s = (Class, UType s)

data UType s
  = UTVar TyVar
  | UTAtm TypeAtom
  | UTApp (UType s) (UType s)
  | UTUni TyVar (UType s)
  | UTCtx (UTypeCstr s) (UType s)
  | UVar (STRef s (UVar s))

makePrisms ''UType

pattern UTFun :: UType s -> UType s -> UType s
pattern UTFun tx ty = UTApp (UTApp (UTAtm TAArr) tx) ty

pattern UTCon :: TyCon -> UType s
pattern UTCon tcon = UTAtm (TACon tcon)

topLevel :: Level
topLevel = Level 0

uvarIds :: [UVarId]
uvarIds = map UVarId [1 ..]

uvarIdName :: UVarId -> Tagged 'TyVar String
uvarIdName (UVarId n) = Tagged ('_':show n)

infixr 1 ~>

(~>) :: UType s -> UType s -> UType s
(~>) = UTFun

unwindUTApp :: UType s -> ST s (UType s, [UType s])
unwindUTApp = go []
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
  TCtx (clss, tc) tq -> UTCtx (clss, open1 tc) (open1 tq)

substUType :: Map TyVar (UType s) -> UType s -> ST s (UType s)
substUType env = go
  where
    go t0 = case t0 of
      UTVar x -> pure (env Map.! x)
      UTAtm{} -> pure t0
      UTApp tf tp -> UTApp <$> go tf <*> go tp
      UTUni{} -> impossible  -- we have only rank-1 types
      UTCtx{} -> impossible
      UVar uref ->
        readSTRef uref >>= \case
          UFree{}  -> pure t0
          ULink t1 -> go  t1

-- * Pretty printing
instance Pretty UVarId where
  pretty = pretty . uvarIdName

prettyUVar :: Int -> UVar s -> ST s Doc
prettyUVar prec = \case
  UFree v _ -> pure (pretty v)
  ULink t   -> prettyUType prec t

prettyUType :: Int -> UType s -> ST s Doc
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
  UTUni v tq -> prettyTUni prec [v] <$> prettyUType 0 tq
  UTCtx (clss, tc) tq -> do
    ptc <- prettyUType 3 tc
    ptq <- prettyUType 0 tq
    pure $ maybeParens (prec > 0) (parens (pretty clss <+> ptc) <+> "=>" <+> ptq)
  UVar uref -> do
    uvar <- readSTRef uref
    prettyUVar prec uvar
