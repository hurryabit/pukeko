{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
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

-- | Declaration of a function signature (@SignDecl Void@) or a method signature
-- (@SignDecl (TScope Int Void)@).
data SignDecl tv = MkSignDecl
  { _sign2name :: Lctd Id.EVar
  , _sign2type :: Type tv
  }

-- | Definition of a function (@FuncDecl lg Void@) or an instance method
-- (@FuncDecl lg (TScope Int Void)@).
data FuncDecl lg tv = MkFuncDecl
  { _func2name :: Lctd Id.EVar
  , _func2type :: TypeOf lg tv
    -- ^ Before type inference, this is 'TArr' and hence bogus. Since 'TArr'
    -- doesn't have kind '*', it will never be there after type inference. This
    -- allows for finding out if we are before or after type inference.
  , _func2body :: Expr lg tv Void
  }

-- | Declaration of an external function.
data ExtnDecl lg = MkExtnDecl
  { _extn2name :: Lctd Id.EVar
  , _extn2type :: TypeOf lg Void
    -- ^ Before type inference, this is 'TArr' for the same reason given for
    -- '_func2type'.
  , _extn2extn :: String
  }

-- | Declaration of a class.
data ClssDecl = MkClssDecl
  { _clss2name    :: Name Clss
  , _clss2param   :: Id.TVar
  , _clss2dcon    :: Name DCon
    -- ^ During type class elimination, we introduce a data constructor for the
    -- dictionary type. The easiest way to get and distribute a 'Name' for this
    -- constructor is to get it during renaming and carry it around afterwards.
    -- For class @XYZ@ this constructor is called @Dict$XZY@.
  , _clss2methods :: [SignDecl (TScope Int Void)]
  }

-- | Definition of an instance of a class.
data InstDecl lg = MkInstDecl
  { _inst2clss    :: Name Clss
  , _inst2atom    :: TypeAtom
  , _inst2qvars   :: [QVar]
  , _inst2methods :: [FuncDecl lg (TScope Int Void)]
  }

data GenDecl (nsp :: Super NameSpace) lg
  = (nsp ?:> TCon)                        => DType  TConDecl
  | (nsp ?:> EVar, IsPreTyped lg ~ False) => DSign (SignDecl Void)
  | (nsp ?:> EVar)                        => DFunc (FuncDecl lg Void)
  | (nsp ?:> EVar)                        => DExtn (ExtnDecl lg)
  | (nsp ?:> Clss, IsClassy   lg ~ True ) => DClss  ClssDecl
  | (nsp ?:> Inst, IsClassy   lg ~ True ) => DInst (InstDecl lg)

type Decl = GenDecl Any

data Module lg = MkModule
  { _module2decls :: [Decl lg]
  }

makeLenses ''SignDecl
makeLenses ''FuncDecl
makeLenses ''ExtnDecl
makeLenses ''ClssDecl
makeLenses ''InstDecl
makePrisms ''GenDecl
makeLenses ''Module


type instance NameSpaceOf ClssDecl = Clss
type instance NameSpaceOf (GenDecl (Only nsp) lg) = nsp

instance HasName  ClssDecl where nameOf = _clss2name
-- FIXME: Replace 'TCon' by 'nsp'.
instance HasName (GenDecl (Only TCon) lg) where
  nameOf = \case
    DType tcon -> nameOf tcon
    DClss clss -> nameOf clss

instance HasPos (SignDecl tv) where getPos = getPos . _sign2name
instance HasPos  ClssDecl     where getPos = getPos . nameOf

instance HasPos (GenDecl nsp st) where
  getPos = \case
    DType tcon  -> getPos tcon
    DSign sign  -> getPos sign
    DFunc func  -> getPos (_func2name func)
    DExtn extn  -> getPos (_extn2name extn)
    DClss clss  -> getPos clss
    DInst inst  -> getPos (_inst2clss inst)

instance BaseTVar tv => Pretty (SignDecl tv) where
  pretty (MkSignDecl name typ_) = pretty (name ::: typ_)

instance (TypeOf lg ~ Type, BaseTVar tv) => Pretty (FuncDecl lg tv) where
  pretty (MkFuncDecl name typ_ body) =
    hang (pretty (name ::: typ_) <+> "=") 2 (pretty body)

instance (TypeOf lg ~ Type) => Pretty (GenDecl nsp lg) where
  pretty = \case
    DType tcon -> "data" <+> pretty tcon
    DSign sign -> pretty sign
    DFunc func -> pretty func
    DExtn (MkExtnDecl name typ_ extn) ->
      hsep ["external", pretty (name ::: typ_), "=", doubleQuotes (pretty extn)]
    DClss (MkClssDecl c v _ ms) ->
      "class" <+> pretty c <+> pretty v <+> "where"
      $$ nest 2 (vcatMap pretty ms)
    DInst (MkInstDecl c t0 qvs ds) ->
      "instance" <+> prettyTypeCstr qvs <+> pretty c <+> prettyPrec 3 t1
      $$ nest 2 (vcatMap pretty ds)
      where
        t1 :: Type (TScope Int Void)
        t1 = mkTApp (TAtm t0) (imap (\i (MkQVar _ v) -> TVar (mkBound i v)) qvs)

instance TypeOf lg ~ Type => Pretty (Module lg) where
  pretty (MkModule decls) = vcatMap pretty decls

deriving instance                   (Show tv) => Show (SignDecl tv)
deriving instance (TypeOf lg ~ Type, Show tv) => Show (FuncDecl lg tv)
deriving instance (TypeOf lg ~ Type)          => Show (ExtnDecl lg)
deriving instance                                Show  ClssDecl
deriving instance (TypeOf lg ~ Type)          => Show (InstDecl lg)
deriving instance (TypeOf lg ~ Type)          => Show (GenDecl nsp lg)
