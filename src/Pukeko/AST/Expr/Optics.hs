module Pukeko.AST.Expr.Optics
  ( freeTmVar
  , substitute
  , patn2binder
  , patn2type
  , arg2expr
  , expr2atom
  , expr2type
  ) where

import Pukeko.Prelude

import           Control.Lens (_Just, prism')
import qualified Data.Set as Set

import           Pukeko.AST.Expr
import           Pukeko.AST.Language
import           Pukeko.AST.Name

substitute' :: Applicative f => (TmVar -> f (Expr lg)) -> Expr lg -> f (Expr lg)
substitute' f = go Set.empty
  where
    go bound = \case
      ELoc l      -> ELoc <$> lctd (go bound) l
      EVar x
        | x `Set.member` bound -> pure (EVar x)
        | otherwise            -> f x
      EAtm a      -> pure (EAtm a)
      EApp e a -> EApp   <$> go bound e <*> arg2expr (go bound) a
      EAbs (TmPar x) e -> ETmAbs x <$> go (Set.insert (nameOf x) bound) e
      EAbs p         e -> EAbs   p <$> go bound e
      ELet m ds e -> ELet m <$> (traverse . b2bound) (go bound0) ds <*> go bound1 e
        where
          bound1 = bound <> setOf (traverse . b2binder . to nameOf) ds
          bound0 = case m of
            BindRec -> bound1
            BindPar -> bound
      EMat t e as -> EMat t <$> go bound e <*> traverse (goAltn bound) as
      ECast coe e -> ECast coe <$> go bound e
      ETyAnn t  e -> ETyAnn t <$> go bound e
    goAltn bound (MkAltn p e) = MkAltn p <$> go (bound <> setOf (patn2binder . _1) p) e

freeTmVar :: Traversal' (Expr lg) TmVar
freeTmVar f = substitute' (fmap EVar . f)

substitute :: (TmVar -> Expr lg) -> Expr lg -> Expr lg
substitute f = runIdentity . substitute' (Identity . f)

patn2binder :: IsNested lg1 ~ IsNested lg2 =>
  Traversal (Patn lg1) (Patn lg2) (TmBinder (TypeOf lg1)) (TmBinder (TypeOf lg2))
patn2binder f = \case
  PWld -> pure PWld
  PVar b -> PVar <$> f b
  PCon c ps -> PCon c <$> (traverse . patn2binder) f ps
  PSimple c bs -> PSimple c <$> (traverse . _Just) f bs

arg2expr :: Prism' (Arg lg) (Expr lg)
arg2expr = prism' TmArg $ \case
  TmArg e -> Just e
  _       -> Nothing

-- | Traverse over all the atoms in an expression.
expr2atom :: Traversal' (Expr lg) Atom
expr2atom f = \case
  ELoc e       -> ELoc <$> lctd (expr2atom f) e
  EVar x       -> pure (EVar x)
  EAtm a       -> EAtm <$> f a
  EApp e a     -> EApp   <$> expr2atom f e <*> (arg2expr . expr2atom) f a
  EAbs p  e    -> EAbs p <$> expr2atom f e
  ELet m ds t  -> ELet m <$> (traverse . b2bound . expr2atom) f ds <*> expr2atom f t
  EMat t e as  -> EMat t <$> expr2atom f e <*> (traverse . altn2expr . expr2atom) f as
  ECast coe e  -> ECast coe <$> expr2atom f e
  ETyAnn t e   -> ETyAnn t <$> expr2atom f e

-- | Traverse over all types in a definition, i.e., the type in the binder on
-- the LHS and the types on the RHS.
b2type :: Traversal' (Bind lg) (TypeOf lg)
b2type f (MkBind b e) = MkBind <$> _2 f b <*> expr2type f e

arg2type :: Traversal' (Arg lg) (TypeOf lg)
arg2type f = \case
  TmArg e  -> TmArg <$> expr2type f e
  TyArg t  -> TyArg <$> f t
  CxArg cx -> CxArg <$> _2 f cx

par2type :: Traversal' (Par lg) (TypeOf lg)
par2type f = \case
  TmPar x  -> TmPar <$> _2 f x
  TyPar v  -> pure (TyPar v)
  CxPar cx -> CxPar <$> _2 f cx

-- | Traverse over all types in an expression.
expr2type :: Traversal' (Expr lg) (TypeOf lg)
expr2type f = \case
  ELoc l       -> ELoc <$> traverse (expr2type f) l
  EVar x       -> pure (EVar x)
  EAtm a       -> pure (EAtm a)
  EApp e  a    -> EApp <$> expr2type f e <*> arg2type f a
  EAbs p  e    -> EAbs <$> par2type f p <*> expr2type f e
  ELet m ds e0 -> ELet m <$> traverse (b2type f) ds <*> expr2type f e0
  EMat t e0 as -> EMat <$> f t <*> expr2type f e0 <*> traverse (altn2type f) as
  ECast (c, t) e -> ECast . (c, ) <$> f t <*> expr2type f e
  ETyAnn t  e0 -> ETyAnn <$> f t <*> expr2type f e0

-- | Traverse over all types in a pattern matching alternative, i.e., the types
-- in the pattern and the types on the RHS.
altn2type :: Traversal' (Altn lg) (TypeOf lg)
altn2type f (MkAltn patn e0) = MkAltn <$> patn2type f patn <*> expr2type f e0

-- | Traverse over all types in a pattern, i.e., the type arguments to the data
-- constructors in the pattern.
patn2type :: IsNested lg1 ~ IsNested lg2 =>
  Traversal (Patn lg1) (Patn lg2) (TypeOf lg1) (TypeOf lg2)
patn2type = patn2binder . _2
