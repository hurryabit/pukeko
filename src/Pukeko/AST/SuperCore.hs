{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Pukeko.AST.SuperCore
  ( module Pukeko.AST.SuperCore
  , module Pukeko.AST.Expr
  ) where

import Pukeko.Prelude
import Pukeko.Pretty

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Map as Map

import           Pukeko.AST.ConDecl
import           Pukeko.AST.Language
import           Pukeko.AST.Expr hiding (Bind, Expr, Altn, Arg, Par)
import qualified Pukeko.AST.Expr as Expr
import           Pukeko.AST.Name
import           Pukeko.AST.Type

data DeclMode = SupC | Extn
type SupC = 'SupC
type Extn = 'Extn

data FuncDecl (m :: Super DeclMode)
  = (m ?:> SupC) =>
    SupCDecl
    { _supc2name   :: TmVar
    , _supc2type   :: GenType Void
    , _supc2params :: [Par]
    , _supc2expr   :: Expr
    }
  | (m ?:> Extn) =>
    ExtnDecl
    { _extn2name :: TmVar
    , _extn2type :: GenType Void
    , _extn2extn :: String
    }

data Module = MkModule
  { _mod2types :: Map TyCon TyConDecl
  , _mod2extns :: Map TmVar (FuncDecl (Only Extn))
  , _mod2supcs :: Map TmVar (FuncDecl (Only SupC))
  }

type Bind = Expr.Bind SuperCore
type Expr = Expr.Expr SuperCore
type Altn = Expr.Altn SuperCore
type Arg  = Expr.Arg  SuperCore
type Par  = Expr.Par  SuperCore

makeLenses ''Module

castAny :: FuncDecl m -> FuncDecl Any
castAny = \case
  SupCDecl z t ps e -> SupCDecl z t ps e
  ExtnDecl z t s    -> ExtnDecl z t s

func2name :: Lens' (FuncDecl m) TmVar
func2name f = \case
  SupCDecl z t ps e -> fmap (\z' -> SupCDecl z' t ps e) (f z)
  ExtnDecl z t s    -> fmap (\z' -> ExtnDecl z' t s)    (f z)

func2type :: Lens' (FuncDecl m) (GenType Void)
func2type f = \case
  SupCDecl z t ps e -> fmap (\t' -> SupCDecl z t' ps e) (f t)
  ExtnDecl z t s    -> fmap (\t' -> ExtnDecl z t' s)    (f t)

func2expr :: Traversal' (FuncDecl m) Expr
func2expr f = \case
  SupCDecl z t ps e -> SupCDecl z t ps <$> f e
  func@ExtnDecl{} -> pure func

mkTypeDecl :: TyConDecl -> Module
mkTypeDecl tcon = over mod2types (Map.insert (nameOf tcon) tcon) mempty

mkFuncDecl :: FuncDecl m -> Module
mkFuncDecl = \case
  SupCDecl z t ps e -> mkDecl mod2supcs z (SupCDecl z t ps e)
  ExtnDecl z t s    -> mkDecl mod2extns z (ExtnDecl z t s)
  where
    mkDecl l k v = over l (Map.insert k v) mempty

type instance NameSpaceOf (FuncDecl m) = 'TmVar
instance HasName (FuncDecl m) where
  nameOf decl = decl ^. func2name
instance HasPos (FuncDecl m) where getPos = getPos . nameOf

instance Semigroup Module where
  MkModule t1 e1 s1 <> MkModule t2 e2 s2 =
    MkModule (t1 <> t2) (e1 <> e2) (s1 <> s2)

instance Monoid Module where
  mempty = MkModule mempty mempty mempty
  mappend = (<>)

instance Pretty (FuncDecl m) where
  pretty = \case
    SupCDecl z t ps e ->
      hang (pretty (z ::: t) <+> "=") 2 (prettyEAbs 0 ps e)
    ExtnDecl z t s ->
      hsep ["external", pretty (z ::: t), "=", doubleQuotes (pretty s)]

instance Pretty Module where
  pretty (MkModule types extns supcs) = vcat
    [ vcatMap (\tcon -> "data" <+> pretty tcon) types
    , vcatMap pretty extns
    , vcatMap pretty supcs
    ]

deriving instance Show (FuncDecl m)
deriving instance Show Module

deriveToJSON defaultOptions ''FuncDecl
deriveToJSON defaultOptions ''Module
