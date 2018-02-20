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

import           Control.Lens (ifoldr)
import           Data.Aeson
import           Data.Aeson.TH

import           Pukeko.Pretty
import qualified Pukeko.AST.Operator   as Op
import           Pukeko.AST.Name
import           Pukeko.AST.Type
import           Pukeko.AST.Language
import           Pukeko.AST.Scope

data Defn lg ev = MkDefn
  { _defn2bind :: Bind (TypeOf lg)
  , _defn2expr :: Expr lg ev
  }

data Atom
  = AVal (Name EVar)
  | ACon (Name DCon)
  | ANum Int

-- NOTE: All constructors added here also NEED TO be added to the COMPLETE
-- pragma below.
data Expr lg ev
  = IsLambda lg ~ True => ELoc (Lctd (Expr lg ev))
  | EVar ev
  | EAtm Atom
  | EApp (Expr lg ev) (Expr lg ev)
  | IsLambda lg ~ True => ELam (Bind (TypeOf lg)) (Expr lg (EScope () ev))
  | ELet [Defn lg             ev ] (Expr lg (EScope Int ev))
  | ERec [Defn lg (EScope Int ev)] (Expr lg (EScope Int ev))
  | EMat (Expr lg ev) (NonEmpty (Altn lg ev))
  | ETyCoe Coercion (Expr lg ev)
  | TypeOf lg ~ Type => ETyAbs (NonEmpty QVar) (Expr lg ev)
  | IsPreTyped lg ~ True => ETyApp (Expr lg ev) (NonEmpty (TypeOf lg))
  | IsPreTyped lg ~ True => ETyAnn (TypeOf lg) (Expr lg ev)

data Bind ty = MkBind
  { _bind2evar :: Name EVar
  , _bind2type :: ty
  }

data Altn lg ev = MkAltn
  { _altn2patn :: Patn lg
  , _altn2expr :: Expr lg (EScope (Name EVar) ev)
  }

data Patn lg
  = IsNested lg ~ True  => PWld
  | IsNested lg ~ True  => PVar    (Name EVar)
  | IsNested lg ~ True  => PCon    (Name DCon) [TypeOf lg] [Patn lg]
  | IsNested lg ~ False => PSimple (Name DCon) [TypeOf lg] [Maybe (Name EVar)]

pattern EVal :: Name EVar -> Expr lg ev
pattern EVal z = EAtm (AVal z)

pattern ECon :: Name DCon -> Expr lg ev
pattern ECon c = EAtm (ACon c)

pattern ENum :: Int -> Expr lg ev
pattern ENum n = EAtm (ANum n)

-- NOTE: Do NOT forget to add new constructors here.
{-# COMPLETE ELoc, EVar, EVal, ECon, ENum, EApp, ELam, ELet, ERec, EMat,
             ETyCoe, ETyAbs, ETyApp, ETyAnn #-}

-- * Derived optics
makeLenses ''Altn
makePrisms ''Atom
makeLenses ''Bind

-- NOTE: The generated lens would not be ploymorphic enough for our use cases.
defn2expr :: (TypeOf lg1 ~ TypeOf lg2) =>
  Lens (Defn lg1 ev1) (Defn lg2 ev2) (Expr lg1 ev1) (Expr lg2 ev2)
defn2expr f (MkDefn b e) = MkDefn b <$> f e

-- * Smart constructors

mkELam ::
  (IsLambda lg ~ True, IsPreTyped lg ~ True, TypeOf lg ~ Type) =>
  [Bind (TypeOf lg)] -> Type -> Expr lg (EScope Int ev) -> Expr lg ev
mkELam bs0 t0 e0 = case bs0 of
  [] -> strengthenE0 e0
  _  -> strengthenE0 (ifoldr (\i b1 -> ELam b1 . fmap (abstr i)) (ETyAnn t0 e0) bs0)
    where
      abstr :: Int -> Scope b Int ev -> Scope b () (Scope b Int ev)
      abstr i = \case
        Bound j b | i == j -> Bound () b
        s                  -> Free s

unwindELam :: (IsLambda lg ~ True) =>
  Expr lg ev -> ([Bind (TypeOf lg)], Expr lg (EScope Int ev))
unwindELam = go 0 [] . weakenE
  where
    inst :: Int -> Scope b () (Scope b Int v) -> Scope b Int v
    inst i = \case
      Bound () b -> Bound i b
      Free s     -> s
    go ::
      Int -> [Bind (TypeOf lg)] -> Expr lg (EScope Int ev) ->
      ([Bind (TypeOf lg)], Expr lg (EScope Int ev))
    go lvl params = \case
      ELam param body -> go (lvl+1) (param:params) (fmap (inst lvl) body)
      -- NOTE: We do this only conditional on the inner expression being a
      -- lambda to not strip the very last type annotation in a chain of
      -- lambdas.
      ETyAnn _ e@ELam{} -> go lvl params e
      body -> (reverse params, body)

unwindEApp :: Expr lg ev -> (Expr lg ev, [Expr lg ev])
unwindEApp = go []
  where
    go args = \case
      EApp fun arg -> go (arg:args) fun
      fun          -> (fun, args)

mkETyApp :: (IsPreTyped lg ~ True) => Expr lg ev -> [TypeOf lg] -> Expr lg ev
mkETyApp e0 = \case
  []    -> e0
  t1:ts -> ETyApp e0 (t1 :| ts)

mkETyAbs :: (TypeOf lg ~ Type) => [QVar] -> Expr lg ev -> Expr lg ev
mkETyAbs qvs0 e0 = case qvs0 of
  []     -> e0
  qv:qvs -> ETyAbs (qv :| qvs) e0

-- * Abstraction and substition

weakenE :: Expr lg ev -> Expr lg (EScope i ev)
weakenE = fmap weakenScope

strengthenE0 :: Expr lg (EScope Int ev) -> Expr lg ev
strengthenE0 = fmap strengthenScope0

-- | Replace subexpressions.
instance Applicative (Expr lg) where
  pure = EVar
  (<*>) = ap

instance Monad (Expr lg) where
  expr >>= f = case expr of
    ELoc l       -> ELoc (fmap (>>= f) l)
    EVar x       -> f x
    EAtm a       -> EAtm a
    EApp e  a    -> EApp (e >>= f) (a >>= f)
    ELam b  e    -> ELam b (e >>>= f)
    ELet ds t    -> ELet (over (traverse . defn2expr) (>>=  f) ds) (t >>>= f)
    ERec ds t    -> ERec (over (traverse . defn2expr) (>>>= f) ds) (t >>>= f)
    EMat t  as   -> EMat (t >>= f) (fmap (over altn2expr (>>>= f)) as)
    ETyCoe c e   -> ETyCoe c (e >>= f)
    ETyAbs _ _   -> error "FIXME: THIS IS NOT IMPLEMENTED"
    ETyApp e t   -> ETyApp (e >>= f) t
    ETyAnn t e   -> ETyAnn t (e >>= f)


-- * Instances

-- ** {Functor, Foldable, Traversable}
deriving instance Functor     (Defn st)
deriving instance Foldable    (Defn st)
deriving instance Traversable (Defn st)

deriving instance Functor     (Expr lg)
deriving instance Foldable    (Expr lg)
deriving instance Traversable (Expr lg)

deriving instance Functor     (Altn lg)
deriving instance Foldable    (Altn lg)
deriving instance Traversable (Altn lg)

type instance NameSpaceOf (Bind ty   ) = EVar
type instance NameSpaceOf (Defn lg ev) = EVar
instance HasName (Bind ty   ) where nameOf = _bind2evar
instance HasName (Defn lg ev) where nameOf = nameOf . _defn2bind
instance HasPos  (Bind ty   ) where getPos = getPos . nameOf
instance HasPos  (Defn lg ev) where getPos = getPos . nameOf


-- * Pretty printing
instance (TypeOf st ~ Type, BaseEVar ev) => Pretty (Defn st ev) where
  pretty (MkDefn b t) =
    hang (pretty b <+> "=") 2 (prettyPrec 0 t)

prettyDefns :: (TypeOf st ~ Type, BaseEVar ev) => Bool -> [Defn st ev] -> Doc ann
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

instance (TypeOf lg ~ Type, BaseEVar ev) => Pretty (Expr lg ev)

instance (TypeOf lg ~ Type, BaseEVar ev) => PrettyPrec (Expr lg ev) where
  prettyPrec prec = \case
    ELoc l -> prettyPrec prec l
    EVar x -> pretty (baseEVar x)
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

prettyELam :: (TypeOf lg ~ Type, BaseEVar ev, Foldable t) =>
  Int -> t (Bind (TypeOf lg)) -> Expr lg (EScope i ev) -> Doc ann
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

instance (TypeOf lg ~ Type, BaseEVar ev) => Pretty (Altn lg ev) where
  pretty (MkAltn p t) = hang ("|" <+> pretty p <+> "->") 2 (pretty t)

instance (TypeOf lg ~ Type) => Pretty (Patn lg)

instance (TypeOf lg ~ Type) => PrettyPrec (Patn lg) where
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

deriving instance (TypeOf lg ~ Type, Show ev) => Show (Defn lg ev)
deriving instance (TypeOf lg ~ Type, Show ev) => Show (Expr lg ev)
deriving instance (TypeOf lg ~ Type, Show ev) => Show (Altn lg ev)
deriving instance                                Show  Atom
deriving instance (TypeOf lg ~ Type)          => Show (Patn lg)
deriving instance                                Show (Bind Type)

deriveToJSON defaultOptions ''Atom

instance (TypeOf lg ~ Type) => ToJSON (Patn lg) where
  toJSON = $(mkToJSON defaultOptions ''Patn)
instance ToJSON (Bind Type) where
  toJSON = $(mkToJSON defaultOptions ''Bind)
instance (ToJSON ev, TypeOf lg ~ Type) => ToJSON (Altn lg ev) where
  toJSON = $(mkToJSON defaultOptions ''Altn)
instance (ToJSON ev, TypeOf lg ~ Type) => ToJSON (Defn lg ev) where
  toJSON = $(mkToJSON defaultOptions ''Defn)
instance (ToJSON ev, TypeOf lg ~ Type) => ToJSON (Expr lg ev) where
  toJSON = $(mkToJSON defaultOptions ''Expr)
