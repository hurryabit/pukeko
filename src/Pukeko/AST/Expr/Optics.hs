module Pukeko.AST.Expr.Optics
  ( expr2atom
  , expr2type
  ) where

import Pukeko.Prelude

import           Pukeko.AST.Expr
import           Pukeko.AST.Language

-- | Traverse over all the atoms on the RHS of a definition.
defn2atom :: Traversal' (Defn lg ev) Atom
defn2atom f (MkDefn b e) = MkDefn b <$> expr2atom f e

-- | Traverse over all the atoms in an expression.
expr2atom :: Traversal' (Expr lg ev) Atom
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
defn2type :: Traversal' (Defn lg ev) (TypeOf lg)
defn2type f (MkDefn b e) = MkDefn <$> bind2type f b <*> expr2type f e

-- | Traverse over all types in an expression.
expr2type :: Traversal' (Expr lg ev) (TypeOf lg)
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
altn2type :: Traversal' (Altn lg ev) (TypeOf lg)
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
