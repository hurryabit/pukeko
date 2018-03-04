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
      ETmApp e  a   -> ETmApp <$> go bound e <*> go bound a
      ETmAbs b  e   -> ETmAbs b <$> go (Set.insert (nameOf b) bound) e
      ELet ds e   -> ELet <$> (traverse . b2bound) (go bound ) ds <*> go bound' e
        where bound' = bound <> setOf (traverse . b2binder . to nameOf) ds
      ERec ds e   -> ERec <$> (traverse . b2bound) (go bound') ds <*> go bound' e
        where bound' = bound <> setOf (traverse . b2binder . to nameOf) ds
      EMat e  as  -> EMat <$> go bound e <*> traverse (goAltn bound) as
      ECast coe e -> ECast coe <$> go bound e
      ETyAbs v  e -> ETyAbs v <$> go bound e
      ETyApp e  t -> ETyApp <$> go bound e <*> pure t
      ETyAnn t  e -> ETyAnn t <$> go bound e
      ECxAbs c  e -> ECxAbs c <$> go bound e
      ECxApp e  c -> ECxApp <$> go bound e <*> pure c
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

-- | Traverse over all the atoms in an expression.
expr2atom :: Traversal' (Expr lg) Atom
expr2atom f = \case
  ELoc e       -> ELoc <$> lctd (expr2atom f) e
  EVar x       -> pure (EVar x)
  EAtm a       -> EAtm <$> f a
  ETmApp e  a    -> ETmApp <$> expr2atom f e <*> expr2atom f a
  ETmAbs bs e    -> ETmAbs bs <$> expr2atom f e
  ELet ds t    -> ELet <$> (traverse . b2bound . expr2atom) f ds <*> expr2atom f t
  ERec ds t    -> ERec <$> (traverse . b2bound . expr2atom) f ds <*> expr2atom f t
  EMat t  as   -> EMat <$> expr2atom f t <*> (traverse . altn2expr . expr2atom) f as
  ECast coe e  -> ECast coe <$> expr2atom f e
  ETyAbs x e   -> ETyAbs x <$> expr2atom f e
  ETyApp e t   -> ETyApp <$> expr2atom f e <*> pure t
  ETyAnn t e   -> ETyAnn t <$> expr2atom f e
  ECxAbs c e   -> ECxAbs c <$> expr2atom f e
  ECxApp e c   -> ECxApp <$> expr2atom f e <*> pure c

-- | Traverse over all types in a definition, i.e., the type in the binder on
-- the LHS and the types on the RHS.
b2type :: Traversal' (Bind lg) (TypeOf lg)
b2type f (MkBind b e) = MkBind <$> _2 f b <*> expr2type f e

-- | Traverse over all types in an expression.
expr2type :: Traversal' (Expr lg) (TypeOf lg)
expr2type f = \case
  ELoc l       -> ELoc <$> traverse (expr2type f) l
  EVar x       -> pure (EVar x)
  EAtm a       -> pure (EAtm a)
  ETmApp e a     -> ETmApp <$> expr2type f e <*> expr2type f a
  ETmAbs b e     -> ETmAbs <$> _2 f b <*> expr2type f e
  ELet ds e0   -> ELet <$> traverse (b2type f) ds <*> expr2type f e0
  ERec ds e0   -> ERec <$> traverse (b2type f) ds <*> expr2type f e0
  EMat e0 as   -> EMat <$> expr2type f e0 <*> traverse (altn2type f) as
  ECast (c, t) e -> ECast . (c, ) <$> f t <*> expr2type f e
  ETyAbs v  e0 -> ETyAbs v <$> expr2type f e0
  ETyApp e0 t  -> ETyApp <$> expr2type f e0 <*> f t
  ETyAnn t  e0 -> ETyAnn <$> f t <*> expr2type f e0
  ECxAbs (c, t) e -> ECxAbs <$> ((c,) <$> f t) <*> expr2type f e
  ECxApp e (c, t) -> ECxApp <$> expr2type f e <*> ((c,) <$> f t)

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
