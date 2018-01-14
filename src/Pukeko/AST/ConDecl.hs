{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
module Pukeko.AST.ConDecl
  ( Some1 (..)
  , Pair1 (..)
  , TConDecl (..)
  , DConDecl (..)
  , typeOf
  ) where

import Pukeko.Prelude

import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Type
import           Pukeko.AST.Scope

data Some1 f = forall n. KnownNat n => Some1 (f n)

data Pair1 f g a = Pair1 (f a) (g a)

data TConDecl n = KnownNat n => MkTConDecl
  { _tname  :: Id.TCon
  , _params :: Vector n Id.TVar
  , _dcons  :: [DConDecl n]
  }

data DConDecl n = MkDConDecl
  { _tcon   :: Id.TCon
  , _dname  :: Id.DCon
  , _tag    :: Int
  , _fields :: [Type (TFinScope n Void)]
  }

typeOf :: TConDecl n -> DConDecl n -> Type Void
typeOf MkTConDecl{_tname, _params} MkDConDecl{_tcon, _dname, _fields}
  | _tname /= _tcon = bugWith "type and data constructor do not match" (_tname, _dname)
  | otherwise       = go _params _fields
  where
    go ::
      forall n1 n2. (KnownNat n1, KnownNat n2) =>
      Vector n1 Id.TVar -> [Type (TFinScope n2 Void)] -> Type Void
    go xs flds =
      case sameNat (Proxy @n1) (Proxy @n2) of
        Just Refl ->
          let res = mkTApp (TCon _tcon) [ TVar (mkBound i x) | (i, x) <- itoList xs ]
          in  mkTUni xs (flds *~> res)
        Nothing -> bug "type and data constructor have different arity" (_tname, _dname)

instance Pretty (TConDecl n) where
  pPrintPrec lvl prec MkTConDecl{_tname} = pPrintPrec lvl prec _tname

instance Pretty (DConDecl n) where
  pPrintPrec lvl prec MkDConDecl{_dname} = pPrintPrec lvl prec _dname

instance Show (Some1 f) where
  show _ = "Some1 ..."
