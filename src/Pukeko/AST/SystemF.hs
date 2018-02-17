{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.AST.SystemF
  ( module Pukeko.AST.SystemF
  , module Pukeko.AST.Expr
  , module Pukeko.AST.Expr.Optics
  ) where

import Pukeko.Prelude
import Pukeko.Pretty

import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Expr
import           Pukeko.AST.Expr.Optics
import           Pukeko.AST.Name
import           Pukeko.AST.Type
import           Pukeko.AST.Language
import           Pukeko.AST.ConDecl

data ClssDecl = MkClssDecl
  { _clss2name    :: Name Clss
  , _clss2param   :: Id.TVar
  , _clss2methods :: [Bind Type (TScope Int Void)]
  }

data InstDecl st = MkInstDecl
  { _inst2clss  :: Name Clss
  , _inst2atom  :: TypeAtom
  , _inst2qvars :: [QVar]
  , _inst2defns :: [Defn st (TScope Int Void) Void]
  }

data ExtnDecl ty = MkExtnDecl
  { _extn2bind :: Bind ty Void
  , _extn2extn :: String
  }

data Decl lg
  =                          DType TConDecl
  | IsPreTyped lg ~ False => DSign (Bind Type Void)
  | IsClassy   lg ~ True  => DClss ClssDecl
  | IsClassy   lg ~ True  => DInst (InstDecl lg)
  |                          DDefn (Defn lg Void Void)
  |                          DExtn (ExtnDecl (TypeOf lg))

data Module lg = MkModule
  { _module2decls :: [Decl lg]
  }

makeLenses ''ClssDecl
makeLenses ''InstDecl
makeLenses ''ExtnDecl
makePrisms ''Decl
makeLenses ''Module

-- * Optics
inst2defn :: Traversal
  (InstDecl st1) (InstDecl st2)
  (Defn st1 (TScope Int Void) Void) (Defn st2 (TScope Int Void) Void)
inst2defn f (MkInstDecl c t qs ds) = MkInstDecl c t qs <$> traverse f ds

-- | Travere over the names of all the functions defined in either a 'DDefn', a
-- 'DSupC' or a 'DExtn'.
decl2func :: Traversal' (Decl st) Id.EVar
decl2func f top = case top of
  DType{} -> pure top
  DSign{} -> pure top
  DClss{} -> pure top
  DInst{} -> pure top
  DDefn d -> DDefn <$> defn2bind (bind2evar (lctd f)) d
  DExtn p -> DExtn <$> extn2bind (bind2evar (lctd f)) p

decl2expr ::
  (Applicative f) =>
  (forall tv ev. Expr st tv ev -> f (Expr st tv ev)) -> Decl st -> f (Decl st)
decl2expr f top = case top of
  DType{} -> pure top
  DSign{} -> pure top
  DClss{} -> pure top
  DInst i -> DInst <$> inst2defn (defn2expr f) i
  DDefn d -> DDefn <$> defn2expr f d
  DExtn p -> pure (DExtn p)

type instance NameSpaceOf ClssDecl = Clss
instance HasName   ClssDecl where nameOf = _clss2name
instance HasPos    ClssDecl where getPos = getPos . nameOf

instance HasPos (Decl st) where
  getPos = \case
    DType tcon  -> getPos tcon
    DSign sign  -> getPos sign
    DClss clss  -> getPos clss
    DInst inst  -> getPos (_inst2clss inst)
    DDefn defn  -> getPos (_defn2bind defn)
    DExtn extn  -> getPos (_extn2bind extn)

instance (PrettyStage st) => Pretty (Decl st) where
  pretty = \case
    DType tcon ->
      "data" <+> pretty tcon
    DSign s -> pretty s
    DClss (MkClssDecl c v ms) ->
      "class" <+> pretty c <+> pretty v <+> "where"
      $$ nest 2 (vcatMap pretty ms)
    DInst (MkInstDecl c t0 qvs ds) ->
      "instance" <+> prettyTypeCstr qvs <+> pretty c <+> prettyPrec 3 t1
      $$ nest 2 (vcatMap pretty ds)
      where
        t1 :: Type (TScope Int Void)
        t1 = mkTApp (TAtm t0) (imap (\i (MkQVar _ v) -> TVar (mkBound i v)) qvs)
    DDefn d -> pretty d
    DExtn (MkExtnDecl b s) ->
      hsep ["external", pretty b, "=", doubleQuotes (pretty s)]

instance (PrettyStage st) => Pretty (Module st) where
  pretty (MkModule decls) = vcatMap pretty decls

deriving instance                                         Show  ClssDecl
deriving instance (TypeOf st ~ Type)                   => Show (InstDecl st)
deriving instance                                         Show (ExtnDecl Type)
deriving instance (TypeOf st ~ Type)                   => Show (Decl st)
