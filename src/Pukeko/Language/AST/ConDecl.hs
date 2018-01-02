module Pukeko.Language.AST.ConDecl
  ( TConDecl (..)
  , DConDecl (..)
  , ConDecls
  , mkTConDecl
  , mkDConDecl
  , mkConDecls
  , getTCon
  , getDCon
  , typeOf
  ) where

import qualified Data.Map              as Map

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
  , _fields :: [Type Closed]
  }

data ConDecls = MkConDecls
  { getTCons :: Map.Map Id.TCon TConDecl
  , getDCons :: Map.Map Id.DCon DConDecl
  }

mkTConDecl :: Id.TCon -> [Id.TVar] -> [DConDecl] -> TConDecl
mkTConDecl _tname _params dcons = MkTConDecl
  { _tname
  , _params
  , _dcons = zipWith (\_tag dcon -> dcon{_tcon = _tname, _tag}) [0..] dcons
  }

mkDConDecl :: Id.DCon -> [Type Closed] -> DConDecl
mkDConDecl _dname _fields =
  -- TODO: Get rid of those @undefined@s.
  MkDConDecl{_tcon = undefined, _dname, _tag = undefined, _fields}

mkConDecls :: Map.Map Id.TCon TConDecl -> Map.Map Id.DCon DConDecl -> ConDecls
mkConDecls tcons dcons = MkConDecls tcons dcons

getTCon :: Id.TCon -> ConDecls -> TConDecl
getTCon tcon decls = getTCons decls Map.! tcon

getDCon :: Id.DCon -> ConDecls -> DConDecl
getDCon dcon decls = getDCons decls Map.! dcon

typeOf :: TConDecl -> DConDecl -> Type Closed
typeOf MkTConDecl{_params} MkDConDecl{_tcon, _fields} =
  foldr (~>) (appTCon _tcon $ map var _params) _fields

instance Pretty TConDecl where
  pPrintPrec lvl prec MkTConDecl{_tname} = pPrintPrec lvl prec _tname

instance Pretty DConDecl where
  pPrintPrec lvl prec MkDConDecl{_dname} = pPrintPrec lvl prec _dname
