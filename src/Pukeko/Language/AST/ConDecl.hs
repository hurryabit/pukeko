{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
module Pukeko.Language.AST.ConDecl
  ( TConDecl (..)
  , DConDecl (..)
  , DConDeclN (..)
  , typeOf
  ) where

import           Data.Foldable     (toList)
import qualified Data.Vector.Sized as Vec

import           Pukeko.Pretty
import qualified Pukeko.Language.Ident as Id
import           Pukeko.Language.Type
import           Pukeko.Language.AST.Scope

data TConDecl = forall n. MkTConDecl
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

-- TODO: Remove this temporary hack.
typeOf :: TConDecl -> DConDecl -> Type Id.TVar
typeOf MkTConDecl{_params} (MkDConDecl MkDConDeclN{_tcon, _fields}) =
  foldr (~>) (appTCon _tcon (map TVar (toList _params))) (map (fmap baseName) _fields)

data DConDecl = forall n. MkDConDecl (DConDeclN n)

instance Pretty TConDecl where
  pPrintPrec lvl prec MkTConDecl{_tname} = pPrintPrec lvl prec _tname

instance Pretty (DConDeclN n) where
  pPrintPrec lvl prec MkDConDeclN{_dname} = pPrintPrec lvl prec _dname

instance Pretty DConDecl where
  pPrintPrec lvl prec (MkDConDecl dcon) = pPrintPrec lvl prec dcon
