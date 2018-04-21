{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.AST.SystemF
  ( module Pukeko.AST.SystemF
  , module Pukeko.AST.Expr
  , module Pukeko.AST.Expr.Optics
  ) where

import Pukeko.Prelude
import Pukeko.Pretty

import Data.Aeson
import Data.Aeson.TH

import           Pukeko.AST.Dict
import           Pukeko.AST.Expr
import           Pukeko.AST.Expr.Optics
import           Pukeko.AST.Name
import           Pukeko.AST.Type
import           Pukeko.AST.Language
import           Pukeko.AST.ConDecl

-- | Declaration of a function signature (@SignDecl Void@) or a method signature
-- (@SignDecl (TScope Int Void)@).
data SignDecl tv = MkSignDecl
  { _sign2name :: TmVar
  , _sign2type :: Type
  }

-- | Definition of a function (@FuncDecl lg Void@) or an instance method
-- (@FuncDecl lg (TScope Int Void)@).
data FuncDecl lg tv = MkFuncDecl
  { _func2name :: TmVar
  , _func2type :: TypeOf lg
    -- ^ Before type inference, this is 'TArr' and hence bogus. Since 'TArr'
    -- doesn't have kind '*', it will never be there after type inference. This
    -- allows for finding out if we are before or after type inference.
  , _func2body :: Expr lg
  }

-- | Declaration of an external function.
data ExtnDecl lg = MkExtnDecl
  { _extn2name :: TmVar
  , _extn2type :: TypeOf lg
    -- ^ Before type inference, this is 'TArr' for the same reason given for
    -- '_func2type'.
  , _extn2extn :: String
  }

-- | Declaration of a class.
data ClassDecl = MkClassDecl
  { _class2name    :: Class
  , _class2param   :: TyVar
  , _class2super   :: Maybe (DxVar, Class)
  , _class2tmcon   :: TmCon
    -- ^ During type class elimination, we introduce a data constructor for the
    -- dictionary type. The easiest way to get and distribute a 'Name' for this
    -- constructor is to get it during renaming and carry it around afterwards.
    -- For class @XYZ@ this constructor is called @Dict$XZY@.
  , _clss2methods :: [SignDecl TyVar]
  }

-- | Definition of an instance of a class.
data InstDecl lg = MkInstDecl
  { _inst2name    :: DxVar
    -- ^ During type class elimination, we introduce a top level value for the
    -- dictionary. We generate the name for this value during renaming, cf.
    -- '_clss2dcon'. For class @XZY@ and type constructor @ABC@ this value is
    -- named @dict$XZY$ABC@.
  , _inst2class   :: Class
  , _inst2type    :: TypeAtom
  , _inst2params  :: [TyVar]
  , _inst2context :: [DxBinder Type]
  , _inst2super   :: Maybe (DictOf lg)
  , _inst2methods :: [FuncDecl lg TyVar]
  }

data GenDecl (nsp :: Super NameSpace) lg
  = (nsp ?:> 'TyCon)                        => DType TyConDecl
  | (nsp ?:> 'TmVar, IsPreTyped lg ~ False) => DSign (SignDecl Void)
  | (nsp ?:> 'TmVar)                        => DFunc (FuncDecl lg Void)
  | (nsp ?:> 'TmVar)                        => DExtn (ExtnDecl lg)
  | (nsp ?:> 'TyCon, IsClassy   lg ~ True ) => DClss ClassDecl
  | (nsp ?:> 'TmVar, IsClassy   lg ~ True ) => DInst (InstDecl lg)

type Decl = GenDecl Any

newtype Module lg = MkModule
  { _module2decls :: [Decl lg]
  }

makeLenses ''SignDecl
makeLenses ''FuncDecl
makeLenses ''ExtnDecl
makeLenses ''ClassDecl
makeLenses ''InstDecl
makePrisms ''GenDecl
makeLenses ''Module

type instance NameSpaceOf (SignDecl    tv) = 'TmVar
type instance NameSpaceOf (FuncDecl lg tv) = 'TmVar
type instance NameSpaceOf (ExtnDecl lg   ) = 'TmVar
type instance NameSpaceOf ClassDecl        = 'TyCon
type instance NameSpaceOf (InstDecl lg   ) = 'TmVar
type instance NameSpaceOf (GenDecl (Only nsp) lg) = nsp

instance HasName (SignDecl    tv) where nameOf = _sign2name
instance HasName (FuncDecl lg tv) where nameOf = _func2name
instance HasName (ExtnDecl lg   ) where nameOf = _extn2name
instance HasName ClassDecl        where nameOf = _class2name
instance HasName (InstDecl lg   ) where nameOf = _inst2name
instance HasName (GenDecl (Only nsp) lg) where
  nameOf = \case
    DType tcon -> nameOf tcon
    DSign sign -> nameOf sign
    DFunc func -> nameOf func
    DExtn extn -> nameOf extn
    DClss clss -> nameOf clss
    DInst inst -> nameOf inst

instance HasPos (SignDecl    tv) where getPos = getPos . nameOf
instance HasPos (FuncDecl lg tv) where getPos = getPos . nameOf
instance HasPos (ExtnDecl lg   ) where getPos = getPos . nameOf
instance HasPos ClassDecl        where getPos = getPos . nameOf
instance HasPos (InstDecl lg   ) where getPos = getPos . nameOf
instance HasPos (GenDecl nsp st) where
  getPos = \case
    DType tcon  -> getPos tcon
    DSign sign  -> getPos sign
    DFunc func  -> getPos func
    DExtn extn  -> getPos extn
    DClss clss  -> getPos clss
    DInst inst  -> getPos inst

instance Pretty (SignDecl tv) where
  pretty (MkSignDecl name typ_) = pretty (name ::: typ_)

instance IsTyped lg => Pretty (FuncDecl lg tv) where
  pretty (MkFuncDecl name typ_ body) =
    hang (pretty (name ::: typ_) <+> "=") 2 (pretty body)

instance IsTyped lg => Pretty (GenDecl nsp lg) where
  pretty = \case
    DType tcon -> "data" <+> pretty tcon
    DSign sign -> pretty sign
    DFunc func -> pretty func
    DExtn (MkExtnDecl name typ_ extn) ->
      hsep ["external", pretty (name ::: typ_), "=", doubleQuotes (pretty extn)]
    DClss (MkClassDecl c v s _ ms) ->
      "class" <+> sdoc <+> pretty c <+> pretty v <+> "where"
      $$ nest 2 (vcatMap pretty ms)
      where
        sdoc = case s of
          Nothing -> mempty
          Just (_, c') -> parens (pretty c' <+> pretty v) <+> "<="
    DInst (MkInstDecl _ c t0 vs ctxt _super ds) ->
      "instance" <+> prettyContext (map snd ctxt) <+> pretty c <+> prettyPrec 3 t1
      $$ nest 2 (vcatMap pretty ds)
      where
        t1 = mkTApp (TAtm t0) (map TVar vs)

instance IsTyped lg => Pretty (Module lg) where
  pretty (MkModule decls) = vcatMap pretty decls

deriving instance             (Show tv) => Show (SignDecl tv)
deriving instance (IsTyped lg, Show tv) => Show (FuncDecl lg tv)
deriving instance (IsTyped lg)          => Show (ExtnDecl lg)
deriving instance                          Show ClassDecl
deriving instance (IsTyped lg)          => Show (InstDecl lg)
deriving instance (IsTyped lg)          => Show (GenDecl nsp lg)


deriveToJSON defaultOptions ''SignDecl
instance IsTyped lg => ToJSON (FuncDecl lg tv) where
  toJSON = $(mkToJSON defaultOptions ''FuncDecl)
instance IsTyped lg => ToJSON (ExtnDecl lg) where
  toJSON = $(mkToJSON defaultOptions ''ExtnDecl)
deriveToJSON defaultOptions ''ClassDecl
instance IsTyped lg => ToJSON (InstDecl lg) where
  toJSON = $(mkToJSON defaultOptions ''InstDecl)

instance IsTyped lg => ToJSON (GenDecl nsp lg) where
  toJSON = $(mkToJSON defaultOptions ''GenDecl)

instance IsTyped lg => ToJSON (Module lg) where
  toJSON = $(mkToJSON defaultOptions ''Module)
