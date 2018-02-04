{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.AST.SystemF
  ( module Pukeko.AST.SystemF
  , module Pukeko.AST.Expr
  , module Pukeko.AST.Expr.Optics
  ) where

import Pukeko.Prelude

import qualified Data.List.NE as NE

import           Pukeko.Pretty
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Expr
import           Pukeko.AST.Expr.Optics
import           Pukeko.AST.Type
import           Pukeko.AST.Language
import           Pukeko.AST.ConDecl

data SignDecl tv = MkSignDecl
  { _sign2func :: Lctd Id.EVar
  , _sign2type :: Type tv
  }

data ClssDecl = MkClssDecl
  { _clss2name  :: Lctd Id.Clss
  , _clss2prm   :: Id.TVar
  , _clss2mthds :: [SignDecl (TScope Int Void)]
  }

data InstDecl st = MkInstDecl
  { _inst2clss  :: Lctd Id.Clss
    -- TODO: Allow type instances for (->).
  , _inst2tcon  :: Id.TCon
  , _inst2qvars :: [QVar]
  , _inst2defns :: [Defn st (TScope Int Void) Void]
  }

data ExtnDecl ty = MkExtnDecl
  { _extn2bind :: Bind ty Void
  , _extn2extn :: String
  }

data Decl lg
  =                          DType (NonEmpty TConDecl)
  | IsPreTyped lg ~ False => DSign (SignDecl Void)
  | IsClassy   lg ~ True  => DClss ClssDecl
  | IsClassy   lg ~ True  => DInst (InstDecl lg)
  |                          DDefn (Defn lg Void Void)
  |                          DExtn (ExtnDecl (TypeOf lg))

data Module lg = MkModule
  { _module2decls :: [Decl lg]
  }

makeLenses ''SignDecl
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

instance HasPos (SignDecl tv) where
  getPos = getPos . _sign2func

instance HasPos (Decl st) where
  getPos = \case
    DType tcons -> getPos (NE.head tcons)
    DSign sign  -> getPos sign
    DClss clss  -> getPos (_clss2name clss)
    DInst inst  -> getPos (_inst2clss inst)
    DDefn defn  -> getPos (_defn2bind defn)
    DExtn extn  -> getPos (_extn2bind extn)

instance (BaseTVar tv) => Pretty (SignDecl tv) where
  pretty (MkSignDecl x t) = pretty x <+> ":" <+> pretty t

instance (PrettyStage st) => Pretty (Decl st) where
  pretty = \case
    DType (dcon0 :| dcons) ->
      "data" <+> pretty dcon0
      $$ vcatMap (\dcon -> "and " <+> pretty dcon) dcons
    DSign s -> pretty s
    DClss (MkClssDecl c v ms) ->
      "class" <+> pretty c <+> pretty v <+> "where"
      $$ nest 2 (vcatMap pretty ms)
    DInst (MkInstDecl c t0 qvs ds) ->
      "instance" <+> prettyTypeCstr qvs <+> pretty c <+> prettyPrec 3 t1
      $$ nest 2 (vcatMap pretty ds)
      where
        t1 :: Type (TScope Int Void)
        t1 = mkTApp (TCon t0) (imap (\i (MkQVar _ v) -> TVar (mkBound i v)) qvs)
    DDefn d -> pretty d
    DExtn (MkExtnDecl b s) ->
      hsep ["external", pretty b, "=", doubleQuotes (pretty s)]

instance (PrettyStage st) => Pretty (Module st) where
  pretty (MkModule decls) = vcatMap pretty decls

deriving instance                   (Show tv)          => Show (SignDecl tv)
deriving instance                                         Show  ClssDecl
deriving instance (TypeOf st ~ Type)                   => Show (InstDecl st)
deriving instance                                         Show (ExtnDecl Type)
deriving instance (TypeOf st ~ Type)                   => Show (Decl st)
