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
import           Pukeko.Pretty

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
  pPrintPrec lvl _ (MkTConDecl tname prms dcons)
    | null dcons =
        pPrintPrec lvl 0 tname <+> hsepMap (pPrintPrec lvl 0) prms
    | otherwise  =
        pPrintPrec lvl 0 tname <+> hsepMap (pPrintPrec lvl 0) prms <+> equals $$
        nest 2 (vcat (map (pPrintPrec lvl 0) dcons))

instance Pretty (DConDecl n) where
  pPrintPrec lvl _ (MkDConDecl _ dname _ flds) =
    "|" <+> pPrintPrec lvl 0 dname <+> hsepMap (pPrintPrec lvl 3) flds

instance Pretty (Some1 TConDecl) where
  pPrintPrec lvl prec (Some1 tcon) = pPrintPrec lvl prec tcon

instance Pretty (Some1 DConDecl) where
  pPrintPrec lvl prec (Some1 dcon) = pPrintPrec lvl prec dcon

instance Show (Some1 TConDecl) where
  show (Some1 tcon) = show tcon

instance Show (Some1 DConDecl) where
  show (Some1 dcon) = show dcon

instance (Show (f a), Show (g a)) => Show (Pair1 f g a) where
  show (Pair1 x y) = show (x, y)

deriving instance Show (TConDecl n)
deriving instance Show (DConDecl n)
