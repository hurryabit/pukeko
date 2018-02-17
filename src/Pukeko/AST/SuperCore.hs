{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE GADTs #-}
module Pukeko.AST.SuperCore
  ( module Pukeko.AST.SuperCore
  , module Pukeko.AST.Expr
  ) where

import Pukeko.Prelude
import Pukeko.Pretty

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Map as Map

import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.ConDecl
import           Pukeko.AST.Language
import           Pukeko.AST.Expr hiding (Defn, Expr, Bind, Altn)
import qualified Pukeko.AST.Expr as Expr
import           Pukeko.AST.Name
import           Pukeko.AST.Type

data DeclMode = SupC | Extn | Any

type family HasSupC (a :: DeclMode) where
  HasSupC SupC = True
  HasSupC Extn = False
  HasSupC Any  = True

type family HasExtn (a :: DeclMode) where
  HasExtn SupC = False
  HasExtn Extn = True
  HasExtn Any  = True

data FuncDecl (m :: DeclMode)
  = HasSupC m ~ True =>
    SupCDecl
    { _supc2bind  :: Bind Void
    , _supc2tprms :: [QVar]
    , _supc2eprms :: [Bind (TScope Int Void)]
    , _supc2expr  :: Expr (TScope Int Void) (EScope Int Void)
    }
  | HasExtn m ~ True =>
    ExtnDecl
    { _extn2bind :: Bind Void
    , _extn2extn :: String
    }

data Module = MkModule
  { _mod2types :: Map (Name TCon) TConDecl
  , _mod2extns :: Map Id.EVar (FuncDecl Extn)
  , _mod2supcs :: Map Id.EVar (FuncDecl SupC)
  }

type Defn = Expr.Defn SuperCore
type Expr = Expr.Expr SuperCore
type Bind = Expr.Bind Type
type Altn = Expr.Altn SuperCore

makeLenses ''Module

castAny :: FuncDecl m -> FuncDecl Any
castAny = \case
  SupCDecl z vs xs e -> SupCDecl z vs xs e
  ExtnDecl z s       -> ExtnDecl z s

func2bind :: Lens' (FuncDecl m) (Bind Void)
func2bind f = \case
  SupCDecl z vs xs e -> fmap (\z' -> SupCDecl z' vs xs e) (f z)
  ExtnDecl z s       -> fmap (\z' -> ExtnDecl z' s) (f z)

func2name :: Lens' (FuncDecl m) Id.EVar
func2name = func2bind . bind2evar . lctd

func2expr :: Traversal' (FuncDecl m) (Expr (TScope Int Void) (EScope Int Void))
func2expr f = \case
  SupCDecl z qvs xs e -> SupCDecl z qvs xs <$> f e
  ExtnDecl z s -> pure (ExtnDecl z s)

mkDecl :: (Ord k) => Lens' Module (Map k v) -> (Lens' v k) -> v -> Module
mkDecl l k v = over l (Map.insert (v^.k) v) mempty

mkTypeDecl :: TConDecl -> Module
mkTypeDecl = mkDecl mod2types tcon2name

mkFuncDecl :: FuncDecl m -> Module
mkFuncDecl = \case
  SupCDecl z vs xs e -> mkDecl mod2supcs func2name (SupCDecl z vs xs e)
  ExtnDecl z s       -> mkDecl mod2extns func2name (ExtnDecl z s)

instance Semigroup Module where
  MkModule t1 e1 s1 <> MkModule t2 e2 s2 =
    MkModule (t1 <> t2) (e1 <> e2) (s1 <> s2)

instance Monoid Module where
  mempty = MkModule mempty mempty mempty
  mappend = (<>)

instance Pretty (FuncDecl m) where
  pretty = \case
    SupCDecl z qvs bs e ->
      hang (pretty z <+> "=") 2 (prettyETyAbs 0 qvs (prettyELam 0 bs e))
    ExtnDecl b s ->
      hsep ["external", pretty b, "=", doubleQuotes (pretty s)]

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
