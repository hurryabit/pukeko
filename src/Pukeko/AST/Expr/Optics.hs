module Pukeko.AST.Expr.Optics where

import Pukeko.Prelude

import           Data.Functor.Compose

import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Expr
import           Pukeko.AST.Language
import           Pukeko.AST.Type

-- | Traverse over all the atoms on the RHS of a definition.
defn2atom :: Traversal' (Defn st tv ev) Atom
defn2atom f (MkDefn b e) = MkDefn b <$> expr2atom f e

-- | Traverse over all the atoms in an expression.
expr2atom :: Traversal' (Expr st tv ev) Atom
expr2atom f = \case
  ELoc e       -> ELoc <$> lctd (expr2atom f) e
  EVar x       -> pure (EVar x)
  EAtm a       -> EAtm <$> f a
  EApp t  us   -> EApp <$> expr2atom f t <*> (traverse . expr2atom) f us
  ECas t  cs   -> ECas <$> expr2atom f t <*> (traverse . case2expr . expr2atom) f cs
  ELam bs e t  -> ELam bs <$> expr2atom f e <*> pure t
  ELet ds t    -> ELet <$> (traverse . defn2atom) f ds <*> expr2atom f t
  ERec ds t    -> ERec <$> (traverse . defn2atom) f ds <*> expr2atom f t
  EMat t  as   -> EMat <$> expr2atom f t <*> (traverse . altn2expr . expr2atom) f as
  ETyCoe d e   -> ETyCoe d <$> expr2atom f e
  ETyAbs x e   -> ETyAbs x <$> expr2atom f e
  ETyApp e t   -> ETyApp <$> expr2atom f e <*> pure t
  ETyAnn t e   -> ETyAnn t <$> expr2atom f e

simplify :: (Applicative f, Functor ty) =>
  (forall s. Traversable s => ty (s tv1) -> f (ty (s tv2))) ->
  ty tv1 -> f (ty tv2)
simplify f = fmap (fmap runIdentity) . f . fmap Identity

scoped :: (Applicative f, Traversable s2, Functor ty) =>
  (forall s1. Traversable s1 => ty (s1 tv1) -> f (ty (s1 tv2))) ->
  (forall s1. Traversable s1 => ty (s1 (s2 tv1)) -> f (ty (s1 (s2 tv2))))
scoped f = fmap (fmap getCompose) . f . fmap Compose

-- | Traverse over all types in a definition, i.e., the type in the binder on
-- the LHS and the types on the RHS.
defn2type :: (Applicative f, Functor (TypeOf lg)) =>
  (forall s. Traversable s => TypeOf lg (s tv1) -> f (TypeOf lg (s tv2))) ->
  Defn lg tv1 ev -> f (Defn lg tv2 ev)
defn2type f (MkDefn b e) = MkDefn <$> bind2type (simplify f) b <*> expr2type f e

-- | Traverse over all types in an expression.
expr2type :: (Applicative f, Functor (TypeOf lg)) =>
  (forall s. Traversable s => TypeOf lg (s tv1) -> f (TypeOf lg (s tv2))) ->
  Expr lg tv1 ev -> f (Expr lg tv2 ev)
expr2type f = \case
  ELoc l       -> ELoc <$> traverse (expr2type f) l
  EVar x       -> pure (EVar x)
  EAtm a       -> pure (EAtm a)
  EApp e0 es   -> EApp <$> expr2type f e0 <*> traverse (expr2type f) es
  ECas e0 cs   -> ECas <$> expr2type f e0 <*> traverse (case2type f ) cs
  ELam bs e t  ->
    ELam <$> traverse (bind2type (simplify f)) bs <*> expr2type f e <*> simplify f t
  ELet ds e0   -> ELet <$> traverse (defn2type f) ds <*> expr2type f e0
  ERec ds e0   -> ERec <$> traverse (defn2type f) ds <*> expr2type f e0
  EMat e0 as   -> EMat <$> expr2type f e0 <*> traverse (altn2type f) as
  ETyCoe c e   -> ETyCoe <$> coercion2type (simplify f) c <*> expr2type f e
  ETyAbs vs e0 -> ETyAbs vs <$> expr2type (scoped f) e0
  ETyApp e0 ts -> ETyApp <$> expr2type f e0 <*> traverse (simplify f) ts
  ETyAnn t  e0 -> ETyAnn <$> simplify f t <*> expr2type f e0

-- TODO: If the binders become typed, we need to traverse them as well.
-- | Traverse over all types in a case alternative, i.e., the type arguments to
-- the data constructor and the types mentioned on the RHS.
case2type :: (Applicative f, Functor (TypeOf lg)) =>
  (forall s. Traversable s => TypeOf lg (s tv1) -> f (TypeOf lg (s tv2))) ->
  Case lg tv1 ev -> f (Case lg tv2 ev)
case2type f (MkCase dcon targs bnds e0) =
  MkCase dcon <$> traverse (simplify f) targs <*> pure bnds <*> expr2type f e0

-- | Traverse over all types in a pattern matching alternative, i.e., the types
-- in the pattern and the types on the RHS.
altn2type :: (Applicative f, Functor (TypeOf lg)) =>
  (forall s. Traversable s => TypeOf lg (s tv1) -> f (TypeOf lg (s tv2))) ->
  Altn lg tv1 ev -> f (Altn lg tv2 ev)
altn2type f (MkAltn patn e0) = MkAltn <$> patn2type f patn <*> expr2type f e0

-- TODO: If the binders become typed, we need to traverse them as well.
-- | Traverse over all types in a pattern, i.e., the type arguments to the data
-- constructors in the pattern.
patn2type :: (Applicative f, Functor ty) =>
  (forall s. Traversable s => ty (s tv1) -> f (ty (s tv2))) ->
  Patn ty tv1 -> f (Patn ty tv2)
patn2type f = \case
  PWld -> pure PWld
  PVar x -> pure (PVar x)
  PCon dcon targs patns ->
    PCon dcon <$> traverse (simplify f) targs <*> traverse (patn2type f) patns

patn2dcon :: Traversal (Patn ty tv) (Patn ty tv) Id.DCon Id.DCon
patn2dcon f = \case
  PWld      -> pure PWld
  PVar x    -> pure (PVar x)
  PCon c ts ps -> PCon <$> f c <*> pure ts <*> (traverse . patn2dcon) f ps

-- | Traverse over all binders in a pattern.
patn2evar :: Traversal' (Patn st tv) Id.EVar
patn2evar f = \case
  PWld      -> pure PWld
  PVar x    -> PVar <$> f x
  PCon c ts ps -> PCon c ts <$> (traverse . patn2evar) f ps

defn2func :: Lens' (Defn st tv ev) Id.EVar
defn2func = defn2bind . bind2evar . lctd
