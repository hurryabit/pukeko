{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Pukeko.Language.AST.ConDecl
  ( TConDecl (..)
  , DConDecl (..)
  , DConDeclN (..)
  , typeOf
  ) where

import           Control.Lens       (itoList)
import           Data.Proxy         (Proxy (Proxy))
import           Data.Type.Equality ((:~:) (Refl))
import qualified Data.Vector.Sized as Vec
import           GHC.TypeLits

import           Pukeko.Error
import           Pukeko.Pretty
import qualified Pukeko.Language.Ident as Id
import           Pukeko.Language.Type
import           Pukeko.Language.AST.Scope

data TConDecl = forall n. KnownNat n => MkTConDecl
  { _tname  :: Id.TCon
  , _params :: Vec.Vector n Id.TVar
  , _dcons  :: [DConDeclN n]
  }

data DConDeclN n = MkDConDeclN
  { _tcon   :: Id.TCon
  , _dname  :: Id.DCon
  , _tag    :: Int
  , _fields :: [Type (TFinScope n Void)]
  }

data DConDecl = forall n. KnownNat n => MkDConDecl (DConDeclN n)

typeOf :: TConDecl -> DConDecl -> Type Void
typeOf MkTConDecl{_tname, _params} (MkDConDecl MkDConDeclN{_tcon, _dname, _fields})
  | _tname /= _tcon = bug "con decl" "type and data constructor do not match" names
  | otherwise       = go _params _fields
  where
    go ::
      forall n1 n2. (KnownNat n1, KnownNat n2) =>
      Vec.Vector n1 Id.TVar -> [Type (TFinScope n2 Void)] -> Type Void
    go xs flds =
      case sameNat (Proxy @n1) (Proxy @n2) of
        Just Refl ->
          let res = appTCon _tcon [ TVar (mkBound i x) | (i, x) <- itoList xs ]
          in  mkTUni xs (flds *~> res)
        Nothing ->
          bug "con decl" "type and data constructor have different arity" names
    names = Just (show _tname ++ " & " ++ show _dname)

instance Pretty TConDecl where
  pPrintPrec lvl prec MkTConDecl{_tname} = pPrintPrec lvl prec _tname

instance Pretty (DConDeclN n) where
  pPrintPrec lvl prec MkDConDeclN{_dname} = pPrintPrec lvl prec _dname

instance Pretty DConDecl where
  pPrintPrec lvl prec (MkDConDecl dcon) = pPrintPrec lvl prec dcon
