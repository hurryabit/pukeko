{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.AST.Expr
  ( Expr (..)
  , Atom (..)
  , Defn (..)
  , Bind (..)
  , Altn (..)
  , Patn (..)

  , pattern EVal
  , pattern ECon
  , pattern ENum

  , defn2bind
  , defn2expr
  , bind2evar
  , bind2type
  , altn2patn
  , altn2expr

  , _AVal

  , mkELam
  , mkETyApp
  , mkETyAbs
  , unwindEApp
  , unwindELam
  , unBind

  , prettyELam
  , prettyETyAbs
  )
  where

import Pukeko.Prelude

import           Control.Lens (makeLensesFor)
import           Data.Aeson
import           Data.Aeson.TH

import           Pukeko.Pretty
import qualified Pukeko.AST.Operator   as Op
import           Pukeko.AST.Name
import           Pukeko.AST.Type
import           Pukeko.AST.Language

data Defn lg = MkDefn
  { _defn2bind :: Bind (TypeOf lg)
  , _defn2expr :: Expr lg
  }

data Atom
  = AVal (Name EVar)
  | ACon (Name DCon)
  | ANum Int

-- NOTE: All constructors added here also NEED TO be added to the COMPLETE
-- pragma below.
data Expr lg
  = IsLambda lg ~ True   => ELoc (Lctd (Expr lg))
  |                         EVar (Name EVar)
  |                         EAtm Atom
  |                         EApp (Expr lg) (Expr lg)
  | IsLambda lg ~ True   => ELam (Bind (TypeOf lg)) (Expr lg)
  |                         ELet [Defn lg] (Expr lg)
  |                         ERec [Defn lg] (Expr lg)
  |                         EMat (Expr lg) (NonEmpty (Altn lg))
  |                         ETyCoe Coercion (Expr lg)
  | TypeOf lg ~ Type     => ETyAbs (NonEmpty QVar) (Expr lg)
  | IsPreTyped lg ~ True => ETyApp (Expr lg) (NonEmpty (TypeOf lg))
  | IsPreTyped lg ~ True => ETyAnn (TypeOf lg) (Expr lg )

data Bind ty = MkBind
  { _bind2evar :: Name EVar
  , _bind2type :: ty
  }

data Altn lg = MkAltn
  { _altn2patn :: Patn lg
  , _altn2expr :: Expr lg
  }

data Patn lg
  = IsNested lg ~ True  => PWld
  | IsNested lg ~ True  => PVar    (Name EVar)
  | IsNested lg ~ True  => PCon    (Name DCon) [TypeOf lg] [Patn lg]
  | IsNested lg ~ False => PSimple (Name DCon) [TypeOf lg] [Maybe (Name EVar)]

pattern EVal :: Name EVar -> Expr lg
pattern EVal z = EAtm (AVal z)

pattern ECon :: Name DCon -> Expr lg
pattern ECon c = EAtm (ACon c)

pattern ENum :: Int -> Expr lg
pattern ENum n = EAtm (ANum n)

-- NOTE: Do NOT forget to add new constructors here.
{-# COMPLETE ELoc, EVar, EVal, ECon, ENum, EApp, ELam, ELet, ERec, EMat,
             ETyCoe, ETyAbs, ETyApp, ETyAnn #-}

-- * Derived optics
makeLenses ''Altn
makePrisms ''Atom
makeLenses ''Bind
makeLensesFor [("_defn2bind", "defn2bind")] ''Defn

-- NOTE: The generated lens would not be polymorphic in @lg@.
defn2expr :: (TypeOf lg1 ~ TypeOf lg2) =>
  Lens (Defn lg1) (Defn lg2) (Expr lg1) (Expr lg2)
defn2expr f (MkDefn b e) = MkDefn b <$> f e

-- * Smart constructors

mkELam ::
  (IsLambda lg ~ True, IsPreTyped lg ~ True, TypeOf lg ~ Type) =>
  [Bind (TypeOf lg)] -> Type -> Expr lg -> Expr lg
mkELam bs0 t0 e0 = case bs0 of
  [] -> e0
  _  -> (foldr ELam (ETyAnn t0 e0) bs0)

unwindELam :: (IsLambda lg ~ True) => Expr lg -> ([Bind (TypeOf lg)], Expr lg)
unwindELam = go []
  where
    go :: [Bind (TypeOf lg)] -> Expr lg -> ([Bind (TypeOf lg)], Expr lg)
    go params = \case
      ELam param body -> go (param:params) body
      -- NOTE: We do this only conditional on the inner expression being a
      -- lambda to not strip the very last type annotation in a chain of
      -- lambdas.
      ETyAnn _ e@ELam{} -> go params e
      body -> (reverse params, body)

unwindEApp :: Expr lg -> (Expr lg, [Expr lg])
unwindEApp = go []
  where
    go args = \case
      EApp fun arg -> go (arg:args) fun
      fun          -> (fun, args)

mkETyApp :: IsPreTyped lg ~ True => Expr lg -> [TypeOf lg] -> Expr lg
mkETyApp e0 = \case
  []    -> e0
  t1:ts -> ETyApp e0 (t1 :| ts)

mkETyAbs :: TypeOf lg ~ Type => [QVar] -> Expr lg -> Expr lg
mkETyAbs qvs0 e0 = case qvs0 of
  []     -> e0
  qv:qvs -> ETyAbs (qv :| qvs) e0

unBind :: Bind ty -> (NameEVar, ty)
unBind (MkBind x t) = (x, t)


-- * Instances

type instance NameSpaceOf (Bind ty) = EVar
type instance NameSpaceOf (Defn lg) = EVar
instance HasName (Bind ty) where nameOf = _bind2evar
instance HasName (Defn lg) where nameOf = nameOf . _defn2bind
instance HasPos  (Bind ty) where getPos = getPos . nameOf
instance HasPos  (Defn lg) where getPos = getPos . nameOf


-- * Pretty printing
instance TypeOf st ~ Type => Pretty (Defn st) where
  pretty (MkDefn b t) =
    hang (pretty b <+> "=") 2 (prettyPrec 0 t)

prettyDefns :: TypeOf st ~ Type => Bool -> [Defn st] -> Doc ann
prettyDefns isrec ds = case ds of
    [] -> impossible  -- maintained invariant
    d0:ds ->
      let_ <+> pretty d0
      $$ vcatMap (\d -> "and" <+> pretty d) ds
    where
      let_ | isrec     = "let rec"
           | otherwise = "let"

instance Pretty Atom where
  pretty = \case
    AVal z -> pretty z
    ACon c -> pretty c
    ANum n -> pretty n

instance TypeOf lg ~ Type => Pretty (Expr lg)

instance TypeOf lg ~ Type => PrettyPrec (Expr lg) where
  prettyPrec prec = \case
    ELoc l -> prettyPrec prec l
    EVar x -> pretty x
    EAtm a -> pretty a
    EApp e a ->
      maybeParens (prec > Op.aprec)
      $ prettyPrec Op.aprec e <+> prettyPrec (Op.aprec+1) a
    -- This could be brought back in @EApp@ when @t@ is an operator.
    -- ApOp   { _op, _arg1, _arg2 } ->
    --   let MkSpec { _sym, _prec, _assoc } = Operator.findByName _op
    --       (prec1, prec2) =
    --         case _assoc of
    --           AssocLeft  -> (_prec  , _prec+1)
    --           AssocRight -> (_prec+1, _prec  )
    --           AssocNone  -> (_prec+1, _prec+1)
    --   in  maybeParens (prec > _prec) $
    --         prettyPrec prec1 _arg1 <> text _sym <> prettyPrec prec2 _arg2
    ELet ds t -> maybeParens (prec > 0) (sep [prettyDefns False ds, "in"] $$ pretty t)
    ERec ds t -> maybeParens (prec > 0) (sep [prettyDefns True  ds, "in"] $$ pretty t)
    -- FIXME: Collect lambdas.
    ELam b e -> prettyELam prec [b] e
    -- If { _cond, _then, _else } ->
    --   maybeParens (prec > 0) $ sep
    --     [ "if"  <+> pretty _cond <+> "then"
    --     , nest 2 (pretty _then)
    --     , "else"
    --     , nest 2 (pretty _else)
    --     ]
    EMat t as ->
      maybeParens (prec > 0) $
        "match" <+> pretty t <+> "with"
        $$ vcatMap pretty as
    ETyCoe (MkCoercion dir tcon) e0 ->
      maybeParens (prec > Op.aprec) $
        "coerce" <+> "@" <> parens (d_from <+> "->" <+> d_to)
        <+> prettyPrec (Op.aprec+1) e0
      where
        (d_from, d_to) = case dir of
          Inject  -> ("_", pretty tcon)
          Project -> (pretty tcon, "_")
    ETyAbs vs e -> prettyETyAbs prec vs (pretty e)
    ETyApp e0 ts ->
      maybeParens (prec > Op.aprec)
      $ prettyPrec Op.aprec e0 <+> prettyAtType (prettyPrec 3) ts
    -- TODO: Decide if it's a good idea to swallow type annotations. Probably,
    -- make a distinction between those given by the user and those generated
    -- during type inference.
    ETyAnn _ e -> prettyPrec prec e

prettyELam :: (TypeOf lg ~ Type, Foldable t) =>
  Int -> t (Bind (TypeOf lg)) -> Expr lg -> Doc ann
prettyELam prec bs e
  | null bs   = prettyPrec prec e
  | otherwise =
      maybeParens (prec > 0)
      $ hang ("fun" <+> hsepMap (prettyPrec 1) bs <+> "->") 2 (pretty e)

prettyETyAbs :: (Foldable t) => Int -> t QVar -> Doc ann -> Doc ann
prettyETyAbs prec qvs d
  | null qvs   = maybeParens (prec > 0) d
  | otherwise =
      maybeParens (prec > 0)
      $ hang ("fun" <+> prettyAtType prettyQVar qvs <+> "->") 2 d

prettyAtType :: Foldable t => (a -> Doc ann) -> t a -> Doc ann
prettyAtType p = hsep . map (\x -> "@" <> p x) . toList

instance Pretty (Bind Type)

instance PrettyPrec (Bind Type) where
  prettyPrec prec (MkBind z t) =
    maybeParens (prec > 0) (pretty z <+> ":" <+> pretty t)

instance TypeOf lg ~ Type => Pretty (Altn lg) where
  pretty (MkAltn p t) = hang ("|" <+> pretty p <+> "->") 2 (pretty t)

instance TypeOf lg ~ Type => Pretty (Patn lg)

instance TypeOf lg ~ Type => PrettyPrec (Patn lg) where
  prettyPrec prec = \case
    PWld -> "_"
    PVar x    -> pretty x
    PCon c ts ps ->
      maybeParens (prec > 0 && (not (null ts) || not (null ps)))
      $ pretty c
        <+> prettyAtType (prettyPrec 3) ts
        <+> hsep (map (prettyPrec 1) ps)
    PSimple c ts bs ->
      maybeParens (prec > 0 && (not (null ts) || not (null bs)))
      $ pretty c
        <+> prettyAtType (prettyPrec 3) ts
        <+> hsep (map (maybe "_" pretty) bs)

deriving instance TypeOf lg ~ Type => Show (Defn lg)
deriving instance TypeOf lg ~ Type => Show (Expr lg)
deriving instance TypeOf lg ~ Type => Show (Altn lg)
deriving instance                     Show  Atom
deriving instance TypeOf lg ~ Type => Show (Patn lg)
deriving instance                                Show (Bind Type)

deriveToJSON defaultOptions ''Atom

instance TypeOf lg ~ Type => ToJSON (Patn lg) where
  toJSON = $(mkToJSON defaultOptions ''Patn)
instance ToJSON (Bind Type) where
  toJSON = $(mkToJSON defaultOptions ''Bind)
instance TypeOf lg ~ Type => ToJSON (Altn lg) where
  toJSON = $(mkToJSON defaultOptions ''Altn)
instance TypeOf lg ~ Type => ToJSON (Defn lg) where
  toJSON = $(mkToJSON defaultOptions ''Defn)
instance TypeOf lg ~ Type => ToJSON (Expr lg) where
  toJSON = $(mkToJSON defaultOptions ''Expr)
