module Pukeko.Language.AST.ConDecl
  ( TConDecl (..)
  , DConDecl (..)
  , mkTConDecl
  , mkDConDecl
  , typeOf
  ) where

import           Pukeko.Pretty
import qualified Pukeko.Language.Ident as Id
import           Pukeko.Language.Type

data TConDecl = MkTConDecl
  { _tname  :: Id.TCon
  , _params :: [Id.TVar]
  , _dcons  :: [DConDecl]
  }

data DConDecl = MkDConDecl
  { _tcon   :: Id.TCon
  , _dname  :: Id.DCon
  , _tag    :: Int
  , _fields :: [Type Id.TVar]
  }

mkTConDecl :: Id.TCon -> [Id.TVar] -> [DConDecl] -> TConDecl
mkTConDecl _tname _params dcons = MkTConDecl
  { _tname
  , _params
  , _dcons = zipWith (\_tag dcon -> dcon{_tcon = _tname, _tag}) [0..] dcons
  }

mkDConDecl :: Id.DCon -> [Type Id.TVar] -> DConDecl
mkDConDecl _dname _fields =
  -- TODO: Get rid of those @undefined@s.
  MkDConDecl{_tcon = undefined, _dname, _tag = undefined, _fields}

typeOf :: TConDecl -> DConDecl -> Type Id.TVar
typeOf MkTConDecl{_params} MkDConDecl{_tcon, _fields} =
  foldr (~>) (appTCon _tcon (map TVar _params)) _fields

instance Pretty TConDecl where
  pPrintPrec lvl prec MkTConDecl{_tname} = pPrintPrec lvl prec _tname

instance Pretty DConDecl where
  pPrintPrec lvl prec MkDConDecl{_dname} = pPrintPrec lvl prec _dname
