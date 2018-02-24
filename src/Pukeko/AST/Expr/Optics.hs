module Pukeko.AST.Expr.Optics
  ( freeEVar
  , subst
  , expr2atom
  , expr2type
  ) where

import Pukeko.Prelude

import           Control.Lens (_Just)
import qualified Data.Set as Set

import           Pukeko.AST.Expr
import           Pukeko.AST.Language
import           Pukeko.AST.Name

subst' :: Applicative f => (NameEVar -> f (Expr lg)) -> Expr lg -> f (Expr lg)
subst' f = go Set.empty
  where
    go bound = \case
      ELoc l      -> ELoc <$> lctd (go bound) l
      EVar x
        | x `Set.member` bound -> pure (EVar x)
        | otherwise            -> f x
      EAtm a      -> pure (EAtm a)
      EApp e  a   -> EApp <$> go bound e <*> go bound a
      ELam b  e   -> ELam b <$> go (Set.insert (_bind2evar b) bound) e
      ELet ds e   -> ELet <$> (traverse . defn2expr) (go bound ) ds <*> go bound' e
        where bound' = bound <> setOf (traverse . defn2bind . bind2evar) ds
      ERec ds e   -> ERec <$> (traverse . defn2expr) (go bound') ds <*> go bound' e
        where bound' = bound <> setOf (traverse . defn2bind . bind2evar) ds
      EMat e  as  -> EMat <$> go bound e <*> traverse (goAltn bound) as
      ETyCoe c  e -> ETyCoe c <$> go bound e
      ETyAbs qv e -> ETyAbs qv <$> go bound e
      ETyApp e  t -> ETyApp <$> go bound e <*> pure t
      ETyAnn t  e -> ETyAnn t <$> go bound e
    goAltn bound (MkAltn p e) = MkAltn p <$> go (bound <> setOf patnEVar p) e

freeEVar :: Traversal' (Expr lg) NameEVar
freeEVar f = subst' (fmap EVar . f)

subst :: (NameEVar -> Expr lg) -> Expr lg -> Expr lg
subst f = runIdentity . subst' (Identity . f)

patnEVar :: Traversal' (Patn lg) NameEVar
patnEVar f = \case
  PWld -> pure PWld
  PVar x -> PVar <$> f x
  PCon c ts ps -> PCon c ts <$> (traverse . patnEVar) f ps
  PSimple c ts bs -> PSimple c ts <$> (traverse . _Just) f bs

-- FIXME: Bring this back as substitution.
-- instance Monad (Expr lg) where
--   expr >>= f = case expr of
--     ELoc l       -> ELoc (fmap (>>= f) l)
--     EVar x       -> f x
--     EAtm a       -> EAtm a
--     EApp e  a    -> EApp (e >>= f) (a >>= f)
--     ELam b  e    -> ELam b (e >>>= f)
--     ELet ds t    -> ELet (over (traverse . defn2expr) (>>=  f) ds) (t >>>= f)
--     ERec ds t    -> ERec (over (traverse . defn2expr) (>>>= f) ds) (t >>>= f)
--     EMat t  as   -> EMat (t >>= f) (fmap (over altn2expr (>>>= f)) as)
--     ETyCoe c e   -> ETyCoe c (e >>= f)
--     ETyAbs _ _   -> error "FIXME: THIS IS NOT IMPLEMENTED"
--     ETyApp e t   -> ETyApp (e >>= f) t
--     ETyAnn t e   -> ETyAnn t (e >>= f)

-- | Traverse over all the atoms on the RHS of a definition.
defn2atom :: Traversal' (Defn lg) Atom
defn2atom f (MkDefn b e) = MkDefn b <$> expr2atom f e

-- | Traverse over all the atoms in an expression.
expr2atom :: Traversal' (Expr lg) Atom
expr2atom f = \case
  ELoc e       -> ELoc <$> lctd (expr2atom f) e
  EVar x       -> pure (EVar x)
  EAtm a       -> EAtm <$> f a
  EApp e  a    -> EApp <$> expr2atom f e <*> expr2atom f a
  ELam bs e    -> ELam bs <$> expr2atom f e
  ELet ds t    -> ELet <$> (traverse . defn2atom) f ds <*> expr2atom f t
  ERec ds t    -> ERec <$> (traverse . defn2atom) f ds <*> expr2atom f t
  EMat t  as   -> EMat <$> expr2atom f t <*> (traverse . altn2expr . expr2atom) f as
  ETyCoe d e   -> ETyCoe d <$> expr2atom f e
  ETyAbs x e   -> ETyAbs x <$> expr2atom f e
  ETyApp e t   -> ETyApp <$> expr2atom f e <*> pure t
  ETyAnn t e   -> ETyAnn t <$> expr2atom f e

-- | Traverse over all types in a definition, i.e., the type in the binder on
-- the LHS and the types on the RHS.
defn2type :: Traversal' (Defn lg) (TypeOf lg)
defn2type f (MkDefn b e) = MkDefn <$> bind2type f b <*> expr2type f e

-- | Traverse over all types in an expression.
expr2type :: Traversal' (Expr lg) (TypeOf lg)
expr2type f = \case
  ELoc l       -> ELoc <$> traverse (expr2type f) l
  EVar x       -> pure (EVar x)
  EAtm a       -> pure (EAtm a)
  EApp e a     -> EApp <$> expr2type f e <*> expr2type f a
  ELam b e     -> ELam <$> bind2type f b <*> expr2type f e
  ELet ds e0   -> ELet <$> traverse (defn2type f) ds <*> expr2type f e0
  ERec ds e0   -> ERec <$> traverse (defn2type f) ds <*> expr2type f e0
  EMat e0 as   -> EMat <$> expr2type f e0 <*> traverse (altn2type f) as
  ETyCoe c e   -> ETyCoe c <$> expr2type f e
  ETyAbs vs e0 -> ETyAbs vs <$> expr2type f e0
  ETyApp e0 ts -> ETyApp <$> expr2type f e0 <*> traverse f ts
  ETyAnn t  e0 -> ETyAnn <$> f t <*> expr2type f e0

-- TODO: If the binders become typed, we need to traverse them as well.
-- | Traverse over all types in a pattern matching alternative, i.e., the types
-- in the pattern and the types on the RHS.
altn2type :: Traversal' (Altn lg) (TypeOf lg)
altn2type f (MkAltn patn e0) = MkAltn <$> patn2type f patn <*> expr2type f e0

-- TODO: If the binders become typed, we need to traverse them as well.
-- | Traverse over all types in a pattern, i.e., the type arguments to the data
-- constructors in the pattern.
patn2type :: Traversal' (Patn lg) (TypeOf lg)
patn2type f = \case
  PWld -> pure PWld
  PVar x -> pure (PVar x)
  PCon dcon targs patns ->
    PCon dcon <$> traverse f targs <*> traverse (patn2type f) patns
  PSimple dcon targs binds ->
    PSimple dcon <$> traverse f targs <*> pure binds
