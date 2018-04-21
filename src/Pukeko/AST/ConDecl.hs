{-# LANGUAGE TemplateHaskell #-}
module Pukeko.AST.ConDecl
  ( TyConDecl (..)
  , TmConDecl (..)
  , tycon2tmcons
  , tmcon2fields
  , typeOfTmCon
  ) where

import Pukeko.Prelude
import Pukeko.Pretty

import           Control.Lens (makeLensesFor, folded, nullOf)
import           Data.Aeson.TH

import           Pukeko.AST.Name
import           Pukeko.AST.Type

-- TODO: We could make it @Type Int@!?
data TyConDecl = MkTyConDecl
  { _tycon2name   :: TyCon
  , _tycon2params :: [TyVar]
  , _tycon2tmcons :: Either Type [TmConDecl]
  }

data TmConDecl = MkTmConDecl
  { _tmcon2tycon  :: TyCon
  , _tmcon2name   :: TmCon
  , _tmcon2tag    :: Int
  , _tmcon2fields :: [Type]
  }

typeOfTmCon :: TyConDecl -> TmConDecl -> Type
typeOfTmCon (MkTyConDecl tycon params _) (MkTmConDecl tyconRef _  _ fields) =
  assert (tycon == tyconRef) $
  let res = rewindl TApp (TCon tycon) (map TVar params)
      typ = rewindr TUni' params (fields *~> res)
  in  assert (nullOf folded typ) typ

type instance NameSpaceOf TyConDecl = 'TyCon
type instance NameSpaceOf TmConDecl = 'TmCon
instance HasName   TyConDecl where nameOf = _tycon2name
instance HasName   TmConDecl where nameOf = _tmcon2name
instance HasPos    TyConDecl where getPos = getPos . nameOf
instance HasPos    TmConDecl where getPos = getPos . nameOf

instance Pretty TyConDecl where
  pretty (MkTyConDecl tname prms dcons0) =
    case dcons0 of
      Left typ -> lhs <+> "=" <+> pretty typ
      Right dcons
        | null dcons -> lhs
        | otherwise  -> lhs <+> "=" $$ nest 2 (vcatMap pretty dcons)
    where
      lhs = pretty tname <+> hsepMap pretty prms

instance Pretty TmConDecl where
  pretty (MkTmConDecl _ dname _ flds) =
    "|" <+> pretty dname <+> hsepMap (prettyPrec 3) flds

deriving instance Show TyConDecl
deriving instance Show TmConDecl

makeLensesFor [("_tycon2tmcons", "tycon2tmcons")] ''TyConDecl
makeLensesFor [("_tmcon2fields", "tmcon2fields")] ''TmConDecl

deriveToJSON defaultOptions ''TyConDecl
deriveToJSON defaultOptions ''TmConDecl
