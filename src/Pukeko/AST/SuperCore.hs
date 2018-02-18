{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
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
import           Pukeko.AST.Expr hiding (Defn, Expr, Bind, Altn)
import qualified Pukeko.AST.Expr as Expr
import           Pukeko.AST.Name
import           Pukeko.AST.Type

data DeclMode = SupC | Extn
type SupC = 'SupC
type Extn = 'Extn

data FuncDecl (m :: Super DeclMode)
  = (m ?:> SupC) =>
    SupCDecl
    { _supc2name  :: Name EVar
    , _supc2type  :: Type Void
    , _supc2tprms :: [QVar]
    , _supc2eprms :: [Bind (TScope Int Void)]
    , _supc2expr  :: Expr (TScope Int Void) (EScope Int Void)
    }
  | (m ?:> Extn) =>
    ExtnDecl
    { _extn2name :: Name EVar
    , _extn2type :: Type Void
    , _extn2extn :: String
    }

data Module = MkModule
  { _mod2types :: Map (Name TCon) TConDecl
  , _mod2extns :: Map (Name EVar) (FuncDecl (Only Extn))
  , _mod2supcs :: Map (Name EVar) (FuncDecl (Only SupC))
  }

type Defn = Expr.Defn SuperCore
type Expr = Expr.Expr SuperCore
type Bind = Expr.Bind Type
type Altn = Expr.Altn SuperCore

makeLenses ''Module

castAny :: FuncDecl m -> FuncDecl Any
castAny = \case
  SupCDecl z t vs xs e -> SupCDecl z t vs xs e
  ExtnDecl z t s       -> ExtnDecl z t s

func2name :: Lens' (FuncDecl m) (Name EVar)
func2name f = \case
  SupCDecl z t vs xs e -> fmap (\z' -> SupCDecl z' t vs xs e) (f z)
  ExtnDecl z t s       -> fmap (\z' -> ExtnDecl z' t s)       (f z)

func2type :: Lens' (FuncDecl m) (Type Void)
func2type f = \case
  SupCDecl z t vs xs e -> fmap (\t' -> SupCDecl z t' vs xs e) (f t)
  ExtnDecl z t s       -> fmap (\t' -> ExtnDecl z t' s)       (f t)

func2expr :: Traversal' (FuncDecl m) (Expr (TScope Int Void) (EScope Int Void))
func2expr f = \case
  SupCDecl z t qvs xs e -> SupCDecl z t qvs xs <$> f e
  func@ExtnDecl{} -> pure func

mkTypeDecl :: TConDecl -> Module
mkTypeDecl tcon = over mod2types (Map.insert (nameOf tcon) tcon) mempty

mkFuncDecl :: FuncDecl m -> Module
mkFuncDecl = \case
  SupCDecl z t vs xs e -> mkDecl mod2supcs z (SupCDecl z t vs xs e)
  ExtnDecl z t s       -> mkDecl mod2extns z (ExtnDecl z t s)
  where
    mkDecl l k v = over l (Map.insert k v) mempty

type instance NameSpaceOf (FuncDecl m) = EVar
instance HasName (FuncDecl m) where
  nameOf decl = decl ^. func2name

instance Semigroup Module where
  MkModule t1 e1 s1 <> MkModule t2 e2 s2 =
    MkModule (t1 <> t2) (e1 <> e2) (s1 <> s2)

instance Monoid Module where
  mempty = MkModule mempty mempty mempty
  mappend = (<>)

instance Pretty (FuncDecl m) where
  pretty = \case
    SupCDecl z t qvs bs e ->
      hang (pretty (z ::: t) <+> "=") 2 (prettyETyAbs 0 qvs (prettyELam 0 bs e))
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
