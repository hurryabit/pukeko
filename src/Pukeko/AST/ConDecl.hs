{-# LANGUAGE ViewPatterns #-}
module Pukeko.AST.ConDecl
  ( TConDecl (..)
  , DConDecl (..)
  , tcon2dcons
  , dcon2fields
  , typeOfDCon
  ) where

import Pukeko.Prelude
import Pukeko.Pretty

import           Control.Lens (makeLensesFor)
import           Data.Aeson.TH

import           Pukeko.AST.Name
import           Pukeko.AST.Type

-- TODO: We could make it @Type Int@!?
data TConDecl = MkTConDecl
  { _tcon2name  :: Name TCon
  , _tcon2prms  :: [NameTVar]
  , _tcon2dcons :: Either Type [DConDecl]
  }

data DConDecl = MkDConDecl
  { _dcon2tcon   :: Name TCon
  , _dcon2name   :: Name DCon
  , _dcon2tag    :: Int
  , _dcon2fields :: [Type]
  }

-- FIXME: Remove 'Void'.
typeOfDCon :: TConDecl -> DConDecl -> GenType Void
typeOfDCon (MkTConDecl tcon tparams _) (MkDConDecl tconRef _  _ fields) =
  assert (tcon == tconRef) $
  let res = rewindl _TApp (TCon tcon) (map TVar tparams)
  in  closeT (rewindr _TUni' tparams (fields *~> res))

type instance NameSpaceOf TConDecl = TCon
type instance NameSpaceOf DConDecl = DCon
instance HasName   TConDecl where nameOf = _tcon2name
instance HasName   DConDecl where nameOf = _dcon2name
instance HasPos    TConDecl where getPos = getPos . nameOf
instance HasPos    DConDecl where getPos = getPos . _dcon2name

instance Pretty TConDecl where
  pretty (MkTConDecl tname prms dcons0) =
    case dcons0 of
      Left typ -> lhs <+> "=" <+> pretty typ
      Right dcons
        | null dcons -> lhs
        | otherwise  -> lhs <+> "=" $$ nest 2 (vcatMap pretty dcons)
    where
      lhs = pretty tname <+> hsepMap pretty prms

instance Pretty DConDecl where
  pretty (MkDConDecl _ dname _ flds) =
    "|" <+> pretty dname <+> hsepMap (prettyPrec 3) flds

deriving instance Show TConDecl
deriving instance Show DConDecl

makeLensesFor [("_tcon2dcons" , "tcon2dcons" )] ''TConDecl
makeLensesFor [("_dcon2fields", "dcon2fields")] ''DConDecl

deriveToJSON defaultOptions ''TConDecl
deriveToJSON defaultOptions ''DConDecl
