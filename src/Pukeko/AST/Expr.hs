{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.AST.Expr
  ( Expr (..)
  , Atom (..)
  , EVarBinder
  , Bind (..)
  , Altn (..)
  , Patn (..)

  , pattern EVal
  , pattern ECon
  , pattern ENum

  , b2binder
  , b2bound
  , altn2patn
  , altn2expr

  , _AVal
  , _ETmApp
  , _ETyApp
  , _ETyAbs
  , _ECxAbs

  , mkETmAbs
  , unwindETmAbs
  , unEAnn

  , prettyETmAbs
  , prettyETyAbs
  )
  where

import Pukeko.Prelude

import           Control.Lens (makeLensesFor)
import           Data.Aeson
import           Data.Aeson.TH
import           Unsafe.Coerce

import           Pukeko.Pretty
import qualified Pukeko.AST.Operator   as Op
import           Pukeko.AST.Name
import           Pukeko.AST.Type
import           Pukeko.AST.Language

data Atom
  = AVal (Name EVar)
  | ACon (Name DCon)
  | ANum Int

type EVarBinder ty = (NameEVar, ty)

data Bind lg = MkBind
  { _b2binder :: EVarBinder (TypeOf lg)
  , _b2bound  :: Expr lg
  }

data Expr lg
  = IsLambda lg ~ True   => ELoc (Lctd (Expr lg))
  |                         EVar (Name EVar)
  |                         EAtm Atom
  |                         ETmApp (Expr lg) (Expr lg)
  | IsLambda lg ~ True   => ETmAbs (EVarBinder (TypeOf lg)) (Expr lg)
  |                         ELet [Bind lg] (Expr lg)
  |                         ERec [Bind lg] (Expr lg)
  |                         EMat (Expr lg) (NonEmpty (Altn lg))
  |                         ECast (Coercion, TypeOf lg) (Expr lg)
  | TypeOf lg ~ Type     => ETyAbs NameTVar (Expr lg)
  | IsPreTyped lg ~ True => ETyApp (Expr lg) (TypeOf lg)
  | (IsLambda lg ~ True, IsPreTyped lg ~ True) => ETyAnn (TypeOf lg) (Expr lg )
  | (IsClassy lg ~ True, TypeOf lg ~ Type) => ECxAbs TypeCstr (Expr lg)
  | (IsClassy lg ~ True, IsPreTyped lg ~ True) => ECxApp (Expr lg) (NameClss, TypeOf lg)

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

-- * Derived optics
makePrisms ''Atom
makePrisms ''Expr
makeLensesFor [("_altn2patn", "altn2patn")]''Altn
makeLensesFor [("_b2binder", "b2binder")] ''Bind

-- NOTE: The generated lens would not be polymorphic in @lg@.
b2bound :: (TypeOf lg1 ~ TypeOf lg2) => Lens (Bind lg1) (Bind lg2) (Expr lg1) (Expr lg2)
b2bound f (MkBind b e) = MkBind b <$> f e

altn2expr :: (TypeOf lg1 ~ TypeOf lg2, IsNested lg1 ~ IsNested lg2) =>
  Lens (Altn lg1) (Altn lg2) (Expr lg1) (Expr lg2)
-- FIXME: This 'unsafeCoerce' must go!
altn2expr f (MkAltn p e) = MkAltn (unsafeCoerce p) <$> f e

-- * Smart constructors

mkETmAbs ::
  (IsLambda lg ~ True, IsPreTyped lg ~ True, TypeOf lg ~ Type) =>
  [EVarBinder (TypeOf lg)] -> Type -> Expr lg -> Expr lg
mkETmAbs bs0 t0 e0 = case bs0 of
  [] -> e0
  _  -> rewindr ETmAbs bs0 (ETyAnn t0 e0)

unwindETmAbs :: (IsLambda lg ~ True) => Expr lg -> ([EVarBinder (TypeOf lg)], Expr lg)
unwindETmAbs = go []
  where
    go :: [EVarBinder (TypeOf lg)] -> Expr lg -> ([EVarBinder (TypeOf lg)], Expr lg)
    go params = \case
      ETmAbs param body -> go (param:params) body
      -- NOTE: We do this only conditional on the inner expression being a
      -- lambda to not strip the very last type annotation in a chain of
      -- lambdas.
      ETyAnn _ e@ETmAbs{} -> go params e
      body -> (reverse params, body)

unEAnn :: Expr lg -> Expr lg
unEAnn = \case
  ETyAnn _ e -> unEAnn e
  e          -> e

-- * Instances

type instance NameSpaceOf (Bind lg) = EVar
instance HasName (Bind lg) where nameOf = nameOf . _b2binder
instance HasPos  (Bind lg) where getPos = getPos . nameOf


-- * Pretty printing
instance TypeOf st ~ Type => Pretty (Bind st) where
  pretty (MkBind (z, t) e) =
    hang (pretty (z ::: t) <+> "=") 2 (prettyPrec 0 e)

prettyDefns :: TypeOf st ~ Type => Bool -> [Bind st] -> Doc ann
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
    ETmApp e a ->
      maybeParens (prec > Op.aprec)
      $ prettyPrec Op.aprec e <+> prettyPrec (Op.aprec+1) a
    ETmAbs b e -> prettyETmAbs prec [b] e
    ELet ds t -> maybeParens (prec > 0) (sep [prettyDefns False ds, "in"] $$ pretty t)
    ERec ds t -> maybeParens (prec > 0) (sep [prettyDefns True  ds, "in"] $$ pretty t)
    -- FIXME: Collect lambdas.
    EMat t as ->
      maybeParens (prec > 0) $
        "match" <+> pretty t <+> "with"
        $$ vcatMap pretty as
    ECast (MkCoercion dir tcon, _typ) e0 ->
      maybeParens (prec > Op.aprec) $
        "coerce" <+> "@" <> parens (d_from <+> "->" <+> d_to)
        <+> prettyPrec (Op.aprec+1) e0
      where
        (d_from, d_to) = case dir of
          Inject  -> ("_", pretty tcon)
          Project -> (pretty tcon, "_")
    e0@ETyAbs{} ->
      let (vs, e1) = unwindr _ETyAbs e0
      in  prettyETyAbs prec vs (pretty e1)
    ETyApp e0 t ->
      maybeParens (prec > Op.aprec)
      $ prettyPrec Op.aprec e0 <+> prettyAtType (prettyPrec 3) [t]
    -- TODO: Decide if it's a good idea to swallow type annotations. Probably,
    -- make a distinction between those given by the user and those generated
    -- during type inference.
    ETyAnn _ e -> prettyPrec prec e
    ECxAbs (clss, typ) e ->
      maybeParens (prec > 0) $
        hang
          ("fun" <+> "?" <> parens (pretty clss <+> prettyPrec 3 typ) <+> "->")
          2 (prettyPrec 0 e)
    ECxApp e0 (clss, typ) ->
      maybeParens (prec > Op.aprec)
      $ prettyPrec Op.aprec e0 <+> "?" <> parens (pretty clss <+> prettyPrec 3 typ)

prettyETmAbs :: (TypeOf lg ~ Type, Foldable t) =>
  Int -> t (EVarBinder (TypeOf lg)) -> Expr lg -> Doc ann
prettyETmAbs prec bs e
  | null bs   = prettyPrec prec e
  | otherwise =
      maybeParens (prec > 0) $
      hang ("fun" <+> hsepMap (prettyPrec 1 . uncurry (:::)) bs <+> "->") 2 (pretty e)

prettyETyAbs :: (Foldable t) => Int -> t NameTVar -> Doc ann -> Doc ann
prettyETyAbs prec vs d
  | null vs   = maybeParens (prec > 0) d
  | otherwise =
      maybeParens (prec > 0)
      $ hang ("fun" <+> prettyAtType pretty vs <+> "->") 2 d

-- FIXME: This should probably be replaced by something more lightweight.
prettyAtType :: Foldable t => (a -> Doc ann) -> t a -> Doc ann
prettyAtType p = hsep . map (\x -> "@" <> p x) . toList

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

deriving instance TypeOf lg ~ Type => Show (Bind lg)
deriving instance TypeOf lg ~ Type => Show (Expr lg)
deriving instance TypeOf lg ~ Type => Show (Altn lg)
deriving instance                     Show  Atom
deriving instance TypeOf lg ~ Type => Show (Patn lg)

deriveToJSON defaultOptions ''Atom

instance TypeOf lg ~ Type => ToJSON (Patn lg) where
  toJSON = $(mkToJSON defaultOptions ''Patn)
instance TypeOf lg ~ Type => ToJSON (Altn lg) where
  toJSON = $(mkToJSON defaultOptions ''Altn)
instance TypeOf lg ~ Type => ToJSON (Bind lg) where
  toJSON = $(mkToJSON defaultOptions ''Bind)
instance TypeOf lg ~ Type => ToJSON (Expr lg) where
  toJSON = $(mkToJSON defaultOptions ''Expr)
