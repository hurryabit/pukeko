{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.AST.Expr
  ( module Pukeko.AST.Expr
  , module Pukeko.AST.Scope
  )
  where

import Pukeko.Prelude

import           Control.Lens (makeLensesFor)
import           Data.Bifoldable
import           Data.Bitraversable

import           Pukeko.Pretty
import qualified Pukeko.AST.Operator   as Op
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Type
import           Pukeko.AST.Language
import           Pukeko.AST.Scope

data Defn st tv ev = MkDefn
  { _defn2bind :: Bind (TypeOf st) tv
  , _defn2expr :: Expr st tv ev
  }

data Atom
  = AVal Id.EVar
  | ACon Id.DCon
  | ANum Int

data Expr lg tv ev
  = IsLambda lg ~ True => ELoc (Lctd (Expr lg tv ev))
  | EVar ev
  | EAtm Atom
  | EApp (Expr lg tv ev) (NonEmpty (Expr lg tv ev))
  | IsLambda lg ~ True =>
    ELam (NonEmpty (Bind (TypeOf lg) tv)) (Expr lg tv (EScope Int ev)) (TypeOf lg tv)
  | ELet [Defn lg tv             ev ] (Expr lg tv (EScope Int ev))
  | ERec [Defn lg tv (EScope Int ev)] (Expr lg tv (EScope Int ev))
  | IsNested lg ~ False => ECas (Expr lg tv ev) (NonEmpty (Case lg tv ev))
  | IsNested lg ~ True  => EMat (Expr lg tv ev) (NonEmpty (Altn lg tv ev))
  | ECoe (Coercion (TypeOf lg tv)) (Expr lg tv ev)
  | IsPreTyped lg ~ True => ETyAbs (NonEmpty QVar) (Expr lg (TScope Int tv) ev)
  | IsPreTyped lg ~ True => ETyApp (Expr lg tv ev) (NonEmpty (TypeOf lg tv))

data Bind ty tv = MkBind
  { _bind2evar :: Lctd Id.EVar
  , _bind2type :: ty tv
  }

data Case st tv ev = MkCase
  { _case2dcon  :: Id.DCon
  , _case2targs :: [TypeOf st tv]
  , _case2binds :: [Maybe Id.EVar]
  , _case2expr  :: Expr st tv (EScope Int ev)
  }

data Altn st tv ev = MkAltn
  { _altn2patn :: Patn (TypeOf st) tv
  , _altn2expr :: Expr st tv (EScope Id.EVar ev)
  }

data Patn ty tv
  = PWld
  | PVar Id.EVar
  | PCon Id.DCon [ty tv] [Patn ty tv]

pattern EVal :: Id.EVar -> Expr st tv ev
pattern EVal z = EAtm (AVal z)

pattern ECon :: Id.DCon -> Expr st tv ev
pattern ECon c = EAtm (ACon c)

pattern ENum :: Int -> Expr st tv ev
pattern ENum n = EAtm (ANum n)

{-# COMPLETE ELoc, EVar, EVal, ECon, ENum, EApp, ELam, ELet, ERec, ECas, EMat, ECoe,
             ETyAbs, ETyApp #-}

-- * Derived optics
makeLenses ''Altn
makePrisms ''Atom
makeLenses ''Bind
makeLenses ''Case
makeLensesFor [("_defn2bind", "defn2bind")] ''Defn

-- NOTE: The generated lens would not be ploymorphic enough for our use cases.
defn2expr ::
  (TypeOf st1 ~ TypeOf st2) =>
  Lens (Defn st1 tv ev1) (Defn st2 tv ev2) (Expr st1 tv ev1) (Expr st2 tv ev2)
defn2expr f (MkDefn b e) = MkDefn b <$> f e

-- * Smart constructors

-- TODO: Fuse nested applications.
mkEApp :: (Foldable t) => Expr st tv ev -> t (Expr st tv ev) -> Expr st tv ev
mkEApp e0 es0 = case toList es0 of
  []    -> e0
  e1:es -> EApp e0 (e1 :| es)

mkELam ::
  (IsLambda st ~ True) =>
  [Bind (TypeOf st) tv]      ->
  Expr st tv (EScope Int ev) ->
  TypeOf st tv               ->
  Expr st tv ev
mkELam bs0 e0 t0 = case bs0 of
  []   -> strengthenE0 e0
  b:bs -> ELam (b :| bs) e0 t0

mkETyApp ::
  (IsPreTyped st ~ True) => Expr st tv ev -> [TypeOf st tv] -> Expr st tv ev
mkETyApp e0 = \case
  []    -> e0
  t1:ts -> ETyApp e0 (t1 :| ts)

mkETyAbs :: (IsLang st, IsPreTyped st ~ True) =>
  [QVar] -> Expr st (TScope Int tv) ev -> Expr st tv ev
mkETyAbs qvs0 e0 = case qvs0 of
  []     -> first strengthenScope0 e0
  qv:qvs -> ETyAbs (qv :| qvs) e0

-- * Abstraction and substition

-- | Abstract all variables which are mapped to @Just@.
abstract :: (ev -> Maybe (i, Id.EVar)) -> Expr st tv ev -> Expr st tv (EScope i ev)
abstract f = fmap (match f)
  where
    match :: (v -> Maybe (i, Id.EVar)) -> v -> EScope i v
    match f v = maybe (Free v) (uncurry mkBound) (f v)

weakenE :: Expr st tv ev -> Expr st tv (EScope i ev)
weakenE = fmap weakenScope

strengthenE0 :: Expr st tv (EScope Int ev) -> Expr st tv ev
strengthenE0 = fmap strengthenScope0

-- | Replace subexpressions.
instance Applicative (Expr st tv) where
  pure = EVar
  (<*>) = ap

instance Monad (Expr st tv) where
  expr >>= f = case expr of
    ELoc l       -> ELoc (fmap (>>= f) l)
    EVar x       -> f x
    EAtm a       -> EAtm a
    EApp t  us   -> EApp (t >>= f) (fmap (>>= f) us)
    ECas t  cs   -> ECas (t >>= f) (fmap (over case2expr (>>>= f)) cs)
    ELam ps e t  -> ELam ps (e >>>= f) t
    ELet ds t    -> ELet (over (traverse . defn2expr) (>>=  f) ds) (t >>>= f)
    ERec ds t    -> ERec (over (traverse . defn2expr) (>>>= f) ds) (t >>>= f)
    EMat t  as   -> EMat (t >>= f) (fmap (over altn2expr (>>>= f)) as)
    ECoe c e     -> ECoe c (e >>= f)
    ETyAbs _ _   -> error "FIXME: THIS IS NOT IMPLEMENTED"
    ETyApp e t   -> ETyApp (e >>= f) t


-- * Instances

-- ** {Functor, Foldable, Traversable}
deriving instance Functor     (Defn st tv)
deriving instance Foldable    (Defn st tv)
deriving instance Traversable (Defn st tv)

deriving instance Functor     (Expr st tv)
deriving instance Foldable    (Expr st tv)
deriving instance Traversable (Expr st tv)

deriving instance Functor     (Case st tv)
deriving instance Foldable    (Case st tv)
deriving instance Traversable (Case st tv)

deriving instance Functor     (Altn st tv)
deriving instance Foldable    (Altn st tv)
deriving instance Traversable (Altn st tv)

deriving instance Functor     ty => Functor     (Patn ty)
deriving instance Foldable    ty => Foldable    (Patn ty)
deriving instance Traversable ty => Traversable (Patn ty)

deriving instance Functor     ty => Functor     (Bind ty)
deriving instance Foldable    ty => Foldable    (Bind ty)
deriving instance Traversable ty => Traversable (Bind ty)

-- ** Bi{functor, foldable, traversable}
instance IsLang st => Bifunctor     (Defn st) where
  bimap = bimapDefault
instance IsLang st => Bifoldable    (Defn st) where
  bifoldMap = bifoldMapDefault
instance IsLang st => Bitraversable (Defn st) where
  bitraverse f g (MkDefn b e) = MkDefn <$> traverse f b <*> bitraverse f g e

instance IsLang st => Bifunctor     (Expr st) where
  bimap = bimapDefault
instance IsLang st => Bifoldable    (Expr st) where
  bifoldMap = bifoldMapDefault
instance IsLang st => Bitraversable (Expr st) where
  bitraverse f g = \case
    ELoc l -> ELoc <$> traverse (bitraverse f g) l
    EVar x -> EVar <$> g x
    EAtm a -> pure (EAtm a)
    EApp e0 es -> EApp <$> bitraverse f g e0 <*> traverse (bitraverse f g) es
    ELam bs e0 t ->
      ELam
      <$> traverse (traverse f) bs
      <*> bitraverse f (traverse g) e0
      <*> traverse f t
    ELet ds e0 ->
      ELet
      <$> traverse (bitraverse f g) ds
      <*> bitraverse f (traverse g) e0
    ERec ds e0 ->
      ERec
      <$> traverse (bitraverse f (traverse g)) ds
      <*> bitraverse f (traverse g) e0
    ECas e0 cs -> ECas <$> bitraverse f g e0 <*> traverse (bitraverse f g) cs
    EMat e0 as -> EMat <$> bitraverse f g e0 <*> traverse (bitraverse f g) as
    ETyAbs v e -> ETyAbs v <$> bitraverse (traverse f) g e
    ETyApp e t -> ETyApp <$> bitraverse f g e <*> traverse (traverse f) t
    ECoe c e   -> ECoe <$> coercion2type (traverse f) c <*> bitraverse f g e

instance IsLang st => Bifunctor     (Case st) where
  bimap = bimapDefault
instance IsLang st => Bifoldable    (Case st) where
  bifoldMap = bifoldMapDefault
instance IsLang st => Bitraversable (Case st) where
  bitraverse f g (MkCase c ts bs e) =
    MkCase c
    <$> traverse (traverse f) ts
    <*> pure bs
    <*> bitraverse f (traverse g) e

instance IsLang st => Bifunctor     (Altn st) where
  bimap = bimapDefault
instance IsLang st => Bifoldable    (Altn st) where
  bifoldMap = bifoldMapDefault
instance IsLang st => Bitraversable (Altn st) where
  bitraverse f g (MkAltn p e) = MkAltn <$> traverse f p <*> bitraverse f (traverse g) e

instance HasPos (Bind ty tv) where
  getPos = getPos . _bind2evar


-- * Pretty printing
class IsType ty => PrettyType ty where
  prettyPrecType :: (BaseTVar tv) => Int -> ty tv -> Doc ann

instance PrettyType NoType where
  prettyPrecType _ NoType = mempty

instance PrettyType Type where
  prettyPrecType = prettyPrec

type PrettyStage st = PrettyType (TypeOf st)

instance (BaseEVar ev, BaseTVar tv, PrettyStage st) => Pretty (Defn st tv ev) where
  pretty (MkDefn b t) =
    hang (pretty b <+> "=") 2 (prettyPrec 0 t)

prettyDefns :: (BaseEVar ev, BaseTVar tv, PrettyStage st) =>
  Bool -> [Defn st tv ev] -> Doc ann
prettyDefns isrec ds = case ds of
    [] -> bug "empty definitions"
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

instance (BaseEVar ev, BaseTVar tv, PrettyStage st) => Pretty (Expr st tv ev)

instance (BaseEVar ev, BaseTVar tv, PrettyStage st) => PrettyPrec (Expr st tv ev) where
  prettyPrec prec = \case
    ELoc l -> prettyPrec prec l
    EVar x -> pretty (baseEVar x)
    EAtm a -> pretty a
    EApp t us ->
      maybeParens (prec > Op.aprec)
      $ prettyPrec Op.aprec t <+> hsepMap (prettyPrec (Op.aprec+1)) us
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
    ELam bs e t -> prettyELamT prec bs e t
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
    ECas t cs ->
      maybeParens (prec > 0) $
        "match" <+> pretty t <+> "with"
        $$ vcatMap pretty cs
    ECoe (MkCoercion dir tcon t_from0 t_to0) e0 ->
      maybeParens (prec > Op.aprec) $
        "coerce" <+> "@" <> parens (d_from <+> "->" <+> d_to)
        <+> prettyPrec (Op.aprec+1) e0
      where
        (d_from, d_to) = case (,) <$> isType t_from0 <*> isType t_to0 of
          Just (t_from, t_to) -> (prettyPrecType 2 t_from, prettyPrecType 1 t_to)
          Nothing -> case dir of
            Inject  -> ("_", pretty tcon)
            Project -> (pretty tcon, "_")
    ETyAbs vs e -> prettyETyAbs prec vs (pretty e)
    ETyApp e0 ts ->
      maybeParens (prec > Op.aprec)
      $ prettyPrec Op.aprec e0 <+> prettyAtType (prettyPrecType 3) ts

prettyELam ::
  (PrettyStage st, BaseTVar tv, BaseEVar ev) =>
  Int -> [Bind (TypeOf st) tv] -> Expr st tv (EScope Int ev) -> Doc ann
prettyELam prec bs e
  | null bs   = prettyPrec prec e
  | otherwise =
      maybeParens (prec > 0)
      $ hang ("fun" <+> hsepMap (prettyPrec 1) bs <+> "->") 2 (pretty e)

prettyELamT ::
  (PrettyStage st, BaseTVar tv, BaseEVar ev) =>
  Int ->
  NonEmpty (Bind (TypeOf st) tv) -> Expr st tv (EScope Int ev) -> TypeOf st tv -> Doc ann
prettyELamT prec bs e t =
  maybeParens (prec > 0)
  $ hang ("fun" <+> hsepMap (prettyPrec 1) bs <+> ":" <+> prettyPrecType 3 t <+> "->") 2 (pretty e)

prettyETyAbs :: (Foldable t) => Int -> t QVar -> Doc ann -> Doc ann
prettyETyAbs prec qvs d
  | null qvs   = maybeParens (prec > 0) d
  | otherwise =
      maybeParens (prec > 0)
      $ hang ("fun" <+> prettyAtType prettyQVar qvs <+> "->") 2 d

prettyAtType :: Foldable t => (a -> Doc ann) -> t a -> Doc ann
prettyAtType p = hsep . map (\x -> "@" <> p x) . toList

instance (BaseTVar tv, PrettyType ty) => Pretty (Bind ty tv)

instance (BaseTVar tv, PrettyType ty) => PrettyPrec (Bind ty tv) where
  prettyPrec prec (MkBind z t0) = case isType t0 of
    Just t  -> maybeParens (prec > 0) (pretty z <+> ":" <+> prettyPrecType 0 t)
    Nothing -> pretty z

instance (BaseEVar ev, BaseTVar tv, PrettyStage st) => Pretty (Case st tv ev) where
  pretty (MkCase c ts bs e) =
    hang
      ( "|"
        <+> pretty c
        <+> prettyAtType (prettyPrecType 3) ts
        <+> hsepMap (maybe "_" pretty) bs <+> "->"
      )
      2 (pretty e)

instance (BaseEVar ev, BaseTVar tv, PrettyStage st) => Pretty (Altn st tv ev) where
  pretty (MkAltn p t) = hang ("|" <+> pretty p <+> "->") 2 (pretty t)

instance (BaseTVar tv, PrettyType ty) => Pretty (Patn ty tv)

instance (BaseTVar tv, PrettyType ty) => PrettyPrec (Patn ty tv) where
  prettyPrec prec = \case
    PWld -> "_"
    PVar x    -> pretty x
    PCon c ts ps ->
      maybeParens (prec > 0 && (not (null ts) || not (null ps)))
      $ pretty c
        <+> prettyAtType (prettyPrecType 3) ts
        <+> hsep (map (prettyPrec 1) ps)

deriving instance (TypeOf st ~ Type, Show tv, Show ev) => Show (Defn st tv ev)
deriving instance (TypeOf st ~ Type, Show tv, Show ev) => Show (Expr st tv ev)
deriving instance (TypeOf st ~ Type, Show tv, Show ev) => Show (Case st tv ev)
deriving instance (TypeOf st ~ Type, Show tv, Show ev) => Show (Altn st tv ev)
deriving instance                                         Show  Atom
deriving instance                   (Show tv)          => Show (Patn Type tv)
deriving instance                   (Show tv)          => Show (Bind Type tv)
