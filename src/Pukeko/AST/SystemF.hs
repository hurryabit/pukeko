{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.AST.SystemF
  ( Module (..)
  , Decl (..)
  , SignDecl (..)
  , ClssDecl (..)
  , InstDecl (..)
  , SupCDecl (..)
  , PrimDecl (..)
  , Defn (..)
  , Expr (..)
  , Bind (..)
  , Case (..)
  , Altn (..)
  , Patn (..)

  , Some1 (..)
  , Pair1 (..)

  , mkEApp
  , mkETyApp

  , abstract
  , (//)

  , module2decls
  , decl2func
  , sign2pos
  , sign2func
  , sign2type
  , inst2defn
  , prim2bind
  , defn2bind
  , defn2expr
  , defn2exprSt
  , defn2dcon
  , expr2pos
  , bind2pos
  , bind2evar
  , bind2type

  , decl2eval
  , expr2eval
  , patn2evar

  , retagDefn

  , Pos
  , module Pukeko.AST.Scope
  )
  where

import Pukeko.Prelude

import Control.Bilens
import Control.Lens   hiding (firstOf)
import Data.Bifoldable
import Data.Bitraversable

import           Pukeko.Pretty
import qualified Pukeko.AST.Operator   as Op
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Type
import           Pukeko.AST.Stage
import           Pukeko.AST.Scope
import           Pukeko.AST.ConDecl

data Module st = MkModule
  { _module2decls :: [Decl st]
  }

data Decl st
  =                         DType (NonEmpty (Some1 TConDecl))
  | Untyped   st ~ 'True => DSign (SignDecl Void)
  | HasLambda st ~ 'True => DClss ClssDecl
  | HasLambda st ~ 'True => DInst (InstDecl st)
  | HasLambda st ~ 'True => DDefn (Defn st Void Void)
  | forall m n. (HasLambda st ~ 'False, StageType st ~ Type, KnownNat m) =>
                            DSupC (SupCDecl st m n)
  |                         DPrim (PrimDecl (StageType st))

data SignDecl tv = MkSignDecl
  { _sign2pos  :: Pos
  , _sign2func :: Id.EVar
  , _sign2type :: Type tv
  }

data ClssDecl = MkClssDecl
  { _clss2pos   :: Pos
  , _clss2name  :: Id.Clss
  , _clss2prm   :: Id.TVar
  , _clss2mthds :: [SignDecl (TFinScope 1 Void)]
  }

data InstDecl st = forall n. MkInstDecl
  { _inst2pos   :: Pos
  , _inst2clss  :: Id.Clss
    -- FIXME: There's no way to define instances on (->).
  , _inst2tcon  :: Id.TCon
  , _inst2qvars :: Vector n QVar
  , _inst2defns :: [Defn st (TFinScope n Void) Void]
  }

data SupCDecl st m n = MkSupCDecl
  { _supc2pos   :: Pos
  , _supc2func  :: Id.EVar
  , _supc2tprms :: Vector m QVar
  , _supc2type  :: StageType st (TFinScope m Void)
  , _supc2eprms :: Vector n (Bind (StageType st) (TFinScope m Void))
  , _supc2expr  :: Expr st (TFinScope m Void) (EFinScope n Void)
  }

data PrimDecl ty = MkPrimDecl
  { _prim2bind :: Bind ty Void
  , _prim2extn :: String
  }

data Defn st tv ev = MkDefn
  { _defn2bind :: Bind (StageType st) tv
  , _defn2expr :: Expr st tv ev
  }

data Expr st tv ev
  = EVar Pos ev
  | EVal Pos Id.EVar
  | ECon Pos Id.DCon
  | ENum Pos Int
  | EApp Pos (Expr st tv ev) (NonEmpty (Expr st tv ev))
  | forall n. HasLambda st ~ 'True =>
    ELam Pos (Vector n (Bind (StageType st) tv)) (Expr st tv (EFinScope n ev)) (StageType st tv)
  | forall n.
    ELet Pos (Vector n (Defn st tv ev)) (Expr st tv (EFinScope n ev))
  | forall n.
    ERec Pos (Vector n (Defn st tv (EFinScope n ev))) (Expr st tv (EFinScope n ev))
  | HasNested st ~ 'False =>
    ECas Pos (Expr st tv ev) (NonEmpty (Case st tv ev))
  | HasNested st ~ 'True =>
    EMat Pos (Expr st tv ev) (NonEmpty (Altn st tv ev))
  | forall m. (HasTypes st ~ 'True, KnownNat m) =>
    ETyAbs Pos (Vector m QVar) (Expr st (TFinScope m tv) ev)
  | HasTypes st ~ 'True =>
    ETyApp Pos (Expr st tv ev) (NonEmpty (StageType st tv))

data Bind ty tv = MkBind
  { _bind2pos  :: Pos
  , _bind2evar :: Id.EVar
  , _bind2type :: ty tv
  }

data Case st tv ev = forall n. MkCase
  { _case2pos   :: Pos
  , _case2dcon  :: Id.DCon
  , _case2targs :: [StageType st tv]
  , _case2binds :: Vector n Id.EVar
  , _case2expr  :: Expr st tv (EFinScope n ev)
  }

data Altn st tv ev = MkAltn
  { _altn2pos  :: Pos
  , _altn2patn :: Patn (StageType st) tv
  , _altn2expr :: Expr st tv (EScope Id.EVar ev)
  }

data Patn ty tv
  = PWld Pos
  | PVar Pos Id.EVar
  | PCon Pos Id.DCon [ty tv] [Patn ty tv]

-- * Derived optics
makeLenses ''Module
makeLenses ''SignDecl
makeLenses ''ClssDecl
makeLenses ''InstDecl
makeLenses ''SupCDecl
makeLenses ''PrimDecl
makeLenses ''Defn
makeLenses ''Bind
makeLenses ''Case
makeLenses ''Altn

mkEApp :: Pos -> Expr st tv ev -> [Expr st tv ev] -> Expr st tv ev
mkEApp w e0 = \case
  []    -> e0
  e1:es -> EApp w e0 (e1 :| es)

mkETyApp ::
  (HasTypes st ~ 'True) => Pos -> Expr st tv ev -> [StageType st tv] -> Expr st tv ev
mkETyApp w e0 = \case
  []    -> e0
  t1:ts -> ETyApp w e0 (t1 :| ts)


-- * Abstraction and substition

-- | Abstract all variables which are mapped to @Just@.
abstract :: (ev -> Maybe (i, Id.EVar)) -> Expr st tv ev -> Expr st tv (EScope i ev)
abstract f = fmap (match f)
  where
    match :: (v -> Maybe (i, Id.EVar)) -> v -> EScope i v
    match f v = maybe (Free v) (uncurry mkBound) (f v)

-- | Replace subexpressions.
(//) :: Expr st tv ev1 -> (Pos -> ev1 -> Expr st tv ev2) -> Expr st tv ev2
expr // f = case expr of
  EVar w x       -> f w x
  EVal w z       -> EVal w z
  ECon w c       -> ECon w c
  ENum w n       -> ENum w n
  EApp w t  us   -> EApp w (t // f) (fmap (// f) us)
  ECas w t  cs   -> ECas w (t // f) (fmap (over' case2expr (/// f)) cs)
  ELam w ps e t  -> ELam w ps (e /// f) t
  ELet w ds t    -> ELet w (over (traverse . defn2expr) (//  f) ds) (t /// f)
  ERec w ds t    -> ERec w (over (traverse . defn2expr) (/// f) ds) (t /// f)
  EMat w t  as   -> EMat w (t // f) (fmap (over altn2expr (/// f)) as)
  ETyAbs _ _ _   -> error "THIS IS NOT IMPLEMENTED"
  ETyApp w e t   -> ETyApp w (e // f) t

(///) ::
  Expr st tv (EScope i ev1) ->
  (Pos -> ev1 -> Expr st tv ev2) ->
  Expr st tv (EScope i ev2)
t /// f = t // (\w x -> pdist w (fmap (f w) x))

pdist :: Pos -> EScope i (Expr st tv ev) -> Expr st tv (EScope i ev)
pdist w (Bound i x) = EVar w (Bound i x)
pdist _ (Free t)    = fmap Free t

-- * Lenses and traversals
module2tops :: Lens (Module st1) (Module st2) [Decl st1] [Decl st2]
module2tops f (MkModule tops) = MkModule <$> f tops

inst2defn ::
  (Applicative f) =>
  (forall n. Defn st1 (TFinScope n Void) Void -> f (Defn st2 (TFinScope n Void) Void)) ->
  InstDecl st1 -> f (InstDecl st2)
inst2defn f (MkInstDecl w c t qs ds) = MkInstDecl w c t qs <$> traverse f ds

-- | Travere over the names of all the functions defined in either a 'DDefn', a
-- 'DSupC' or a 'DPrim'.
decl2func :: Traversal' (Decl st) Id.EVar
decl2func f top = case top of
  DType{} -> pure top
  DSign{} -> pure top
  DClss{} -> pure top
  DInst{} -> pure top
  DDefn d -> DDefn <$> defn2bind (bind2evar f) d
  DSupC s -> DSupC <$> supc2func f s
  DPrim p -> DPrim <$> prim2bind (bind2evar f) p

decl2expr ::
  (Applicative f) =>
  (forall tv ev. Expr st tv ev -> f (Expr st tv ev)) -> Decl st -> f (Decl st)
decl2expr f top = case top of
  DType{} -> pure top
  DSign{} -> pure top
  DClss{} -> pure top
  DInst i -> DInst <$> inst2defn (defn2expr f) i
  DDefn d -> DDefn <$> defn2expr f d
  DSupC s -> DSupC <$> supc2expr f s
  DPrim p -> pure (DPrim p)

decl2eval :: IndexedTraversal' Pos (Decl st) Id.EVar
decl2eval f = decl2expr (expr2eval f)

defn2exprSt ::
  (StageType st1 ~ StageType st2) =>
  Lens (Defn st1 tv ev1) (Defn st2 tv ev2) (Expr st1 tv ev1) (Expr st2 tv ev2)
defn2exprSt f (MkDefn b e) = MkDefn b <$> f e


-- * Deep traversals
type DConTraversal t =
  forall st1 st2 tv ev. (SameNodes st1 st2, SameTypes st1 st2) =>
  IndexedTraversal Pos (t st1 tv ev) (t st2 tv ev) Id.DCon Id.DCon

defn2dcon :: DConTraversal Defn
defn2dcon f (MkDefn b e) = MkDefn b <$> expr2dcon f e

expr2dcon :: DConTraversal Expr
expr2dcon f = \case
  EVar w x       -> pure (EVar w x)
  EVal w z       -> pure (EVal w z)
  ECon w c       -> ECon w <$> indexed f w c
  ENum w n       -> pure (ENum w n)
  EApp w t  us   -> EApp w <$> expr2dcon f t <*> (traverse . expr2dcon) f us
  ECas w t  cs   -> ECas w <$> expr2dcon f t <*> (traverse . case2dconIx) f cs
  ELam w bs e t  -> ELam w bs <$> expr2dcon f e <*> pure t
  ELet w ds t    -> ELet w <$> (traverse . defn2dcon) f ds <*> expr2dcon f t
  ERec w ds t    -> ERec w <$> (traverse . defn2dcon) f ds <*> expr2dcon f t
  EMat w t  as   -> EMat w <$> expr2dcon f t <*> (traverse . altn2dcon) f as
  ETyAbs w x e   -> ETyAbs w x <$> expr2dcon f e
  ETyApp w e t   -> ETyApp w <$> expr2dcon f e <*> pure t

expr2pos :: Lens' (Expr st tv ev) Pos
expr2pos f = \case
    EVar w x       -> fmap (\w' -> EVar w' x      ) (f w)
    EVal w z       -> fmap (\w' -> EVal w' z      ) (f w)
    ECon w c       -> fmap (\w' -> ECon w' c      ) (f w)
    ENum w n       -> fmap (\w' -> ENum w' n      ) (f w)
    EApp w t  us   -> fmap (\w' -> EApp w' t  us  ) (f w)
    ECas w t  cs   -> fmap (\w' -> ECas w' t  cs  ) (f w)
    ELam w ps e t  -> fmap (\w' -> ELam w' ps e t ) (f w)
    ELet w ds t    -> fmap (\w' -> ELet w' ds t   ) (f w)
    ERec w ds t    -> fmap (\w' -> ERec w' ds t   ) (f w)
    EMat w ts as   -> fmap (\w' -> EMat w' ts as  ) (f w)
    ETyAbs w x e   -> fmap (\w' -> ETyAbs w' x e  ) (f w)
    ETyApp w e t   -> fmap (\w' -> ETyApp w' e t  ) (f w)

case2dconIx :: DConTraversal Case
case2dconIx f (MkCase w c ts bs e) =
  MkCase w <$> indexed f w c <*> pure ts <*> pure bs <*> expr2dcon f e

altn2dcon :: DConTraversal Altn
altn2dcon f (MkAltn w p t) =
  MkAltn w <$> patn2dcon f p <*> expr2dcon f t

patn2dcon :: IndexedTraversal Pos (Patn ty tv) (Patn ty tv) Id.DCon Id.DCon
patn2dcon f = \case
  PWld w      -> pure (PWld w)
  PVar w x    -> pure (PVar w x)
  PCon w c ts ps -> PCon w <$> indexed f w c <*> pure ts <*> (traverse . patn2dcon) f ps

type EValTraversal t =
  forall st1 st2 tv ev. (SameNodes st1 st2, SameTypes st1 st2) =>
  IndexedTraversal Pos (t st1 tv ev) (t st2 tv ev) Id.EVar Id.EVar

expr2eval :: EValTraversal Expr
expr2eval f = \case
  EVar w x       -> pure (EVar w x)
  EVal w z       -> EVal w <$> indexed f w z
  ECon w c       -> pure (ECon w c)
  ENum w n       -> pure (ENum w n)
  EApp w t  us   -> EApp w <$> expr2eval f t <*> (traverse . expr2eval) f us
  ECas w t  cs   -> ECas w <$> expr2eval f t <*> (traverse . case2eval) f cs
  ELam w bs e t  -> ELam w bs <$> expr2eval f e <*> pure t
  ELet w ds t    ->
    ELet w <$> (traverse . defn2exprSt . expr2eval) f ds <*> expr2eval f t
  ERec w ds t    ->
    ERec w <$> (traverse . defn2exprSt . expr2eval) f ds <*> expr2eval f t
  EMat w t  as   -> EMat w <$> expr2eval f t <*> (traverse . altn2eval) f as
  ETyAbs w x e   -> ETyAbs w x <$> expr2eval f e
  ETyApp w e t   -> ETyApp w <$> expr2eval f e <*> pure t

case2eval :: EValTraversal Case
case2eval f (MkCase w c ts bs e) = MkCase w c ts bs <$> expr2eval f e

altn2eval :: EValTraversal Altn
altn2eval f (MkAltn w p t) = MkAltn w p <$> expr2eval f t

patn2evar :: IndexedTraversal' Pos (Patn st tv) Id.EVar
patn2evar f = \case
  PWld w      -> pure (PWld w)
  PVar w x    -> PVar w <$> indexed f w x
  PCon w c ts ps -> PCon w c ts <$> (traverse . patn2evar) f ps

-- * Scoped lenses
type EScopedLens s t a b =
  forall f ev1 ev2. (Functor f) =>
  (forall i. a (EScope i ev1) -> f (b (EScope i ev2))) -> s ev1 -> f (t ev2)

over' ::
  ((forall i. g (s i v1) -> Identity (g (s i v2))) -> f v1 -> Identity (f v2)) ->
   (forall i. g (s i v1) ->           g (s i v2))  -> f v1 ->           f v2
over' l f = runIdentity . l (Identity . f)

case2expr :: EScopedLens (Case st tv) (Case st tv) (Expr st tv) (Expr st tv)
case2expr f (MkCase w c ts bs e) = MkCase w c ts bs <$> f e

-- * Retagging
retagDefn :: (SameNodes st1 st2, SameTypes st1 st2) => Defn st1 tv ev -> Defn st2 tv ev
retagDefn = over defn2dcon id


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

-- * {Functor,Foldable,Traversable}WithIndex
instance IsStage st => FunctorWithIndex     Pos (Defn st tv) where
instance IsStage st => FoldableWithIndex    Pos (Defn st tv) where
instance IsStage st => TraversableWithIndex Pos (Defn st tv) where
  itraverse = ibitraverse (const pure)

instance IsStage st => FunctorWithIndex     Pos (Expr st tv) where
instance IsStage st => FoldableWithIndex    Pos (Expr st tv) where
instance IsStage st => TraversableWithIndex Pos (Expr st tv) where
  itraverse = ibitraverse (const pure)

instance IsStage st => FunctorWithIndex     Pos (Case st tv) where
instance IsStage st => FoldableWithIndex    Pos (Case st tv) where
instance IsStage st => TraversableWithIndex Pos (Case st tv) where
  itraverse = ibitraverse (const pure)

instance IsStage st => FunctorWithIndex     Pos (Altn st tv) where
instance IsStage st => FoldableWithIndex    Pos (Altn st tv) where
instance IsStage st => TraversableWithIndex Pos (Altn st tv) where
  itraverse = ibitraverse (const pure)

instance TraversableWithIndex Pos ty => FunctorWithIndex     Pos (Patn ty) where
instance TraversableWithIndex Pos ty => FoldableWithIndex    Pos (Patn ty) where
instance TraversableWithIndex Pos ty => TraversableWithIndex Pos (Patn ty) where
  itraverse f = \case
    PWld w   -> pure (PWld w)
    PVar w x -> pure (PVar w x)
    PCon w c ts ps ->
      PCon w c
      <$> traverse (itraverse f) ts
      <*> traverse (itraverse f) ps

instance TraversableWithIndex Pos ty => FunctorWithIndex     Pos (Bind ty) where
instance TraversableWithIndex Pos ty => FoldableWithIndex    Pos (Bind ty) where
instance TraversableWithIndex Pos ty => TraversableWithIndex Pos (Bind ty) where
  itraverse f (MkBind w x t) = MkBind w x <$> itraverse f t

-- ** Bi{functor, foldable, traversable}
instance IsStage st => Bifunctor     (Defn st) where
  bimap = bimapDefault
instance IsStage st => Bifoldable    (Defn st) where
  bifoldMap = bifoldMapDefault
instance IsStage st => Bitraversable (Defn st) where
  bitraverse f g = ibitraverse (const f) (const g)

instance IsStage st => Bifunctor     (Expr st) where
  bimap = bimapDefault
instance IsStage st => Bifoldable    (Expr st) where
  bifoldMap = bifoldMapDefault
instance IsStage st => Bitraversable (Expr st) where
  bitraverse f g = ibitraverse (const f) (const g)

instance IsStage st => Bifunctor     (Case st) where
  bimap = bimapDefault
instance IsStage st => Bifoldable    (Case st) where
  bifoldMap = bifoldMapDefault
instance IsStage st => Bitraversable (Case st) where
  bitraverse f g = ibitraverse (const f) (const g)

instance IsStage st => Bifunctor     (Altn st) where
  bimap = bimapDefault
instance IsStage st => Bifoldable    (Altn st) where
  bifoldMap = bifoldMapDefault
instance IsStage st => Bitraversable (Altn st) where
  bitraverse f g = ibitraverse (const f) (const g)

-- ** Bi{functor, foldable, traversable}WithIndex
instance IsStage st => BifunctorWithIndex     Pos (Defn st) where
instance IsStage st => BifoldableWithIndex    Pos (Defn st) where
instance IsStage st => BitraversableWithIndex Pos (Defn st) where
  ibitraverse f g (MkDefn b e) = MkDefn <$> itraverse f b <*> ibitraverse f g e

instance IsStage st => BifunctorWithIndex     Pos (Expr st) where
instance IsStage st => BifoldableWithIndex    Pos (Expr st) where
instance IsStage st => BitraversableWithIndex Pos (Expr st) where
  ibitraverse f g = \case
    EVar w x -> EVar w <$> g w x
    EVal w z -> pure (EVal w z)
    ECon w c -> pure (ECon w c)
    ENum w n -> pure (ENum w n)
    EApp w e0 es ->
      EApp w
      <$> ibitraverse f g e0
      <*> traverse (ibitraverse f g) es
    ELam w bs e0 t ->
      ELam w
      <$> traverse (itraverse f) bs
      <*> ibitraverse f (traverse . g) e0
      <*> itraverse f t
    ELet w ds e0 ->
      ELet w
      <$> traverse (ibitraverse f g) ds
      <*> ibitraverse f (traverse . g) e0
    ERec w ds e0 ->
      ERec w
      <$> traverse (ibitraverse f (traverse . g)) ds
      <*> ibitraverse f (traverse . g) e0
    ECas w e0 cs ->
      ECas w
      <$> ibitraverse f g e0
      <*> traverse (ibitraverse f g) cs
    EMat w e0 as ->
      EMat w
      <$> ibitraverse f g e0
      <*> traverse (ibitraverse f g) as
    ETyAbs w v e -> ETyAbs w v <$> ibitraverse (traverse . f) g e
    ETyApp w e t ->
      ETyApp w
      <$> ibitraverse f g e
      <*> traverse (itraverse f) t

instance IsStage st => BifunctorWithIndex     Pos (Case st) where
instance IsStage st => BifoldableWithIndex    Pos (Case st) where
instance IsStage st => BitraversableWithIndex Pos (Case st) where
  ibitraverse f g (MkCase w c ts bs e) =
    MkCase w c
    <$> traverse (itraverse f) ts
    <*> pure bs
    <*> ibitraverse f (traverse . g) e

instance IsStage st => BifunctorWithIndex     Pos (Altn st) where
instance IsStage st => BifoldableWithIndex    Pos (Altn st) where
instance IsStage st => BitraversableWithIndex Pos (Altn st) where
  ibitraverse f g (MkAltn w p e) =
    MkAltn w <$> itraverse f p <*> ibitraverse f (traverse . g) e

-- * Pretty printing
class PrettyType f where
  pPrintPrecType :: (BaseTVar tv) => PrettyLevel -> Rational -> f tv -> Doc

instance PrettyType NoType where
  pPrintPrecType _ _ NoType = mempty

instance PrettyType Type where
  pPrintPrecType = pPrintPrec

type PrettyStage st = PrettyType (StageType st)

instance (PrettyStage st) => Pretty (Module st) where
  pPrintPrec lvl prec (MkModule tops) = vcat (map (pPrintPrec lvl prec) tops)

instance (PrettyStage st) => Pretty (Decl st) where
  pPrintPrec lvl _ = \case
    DType (dcon0 :| dcons) ->
      "type" <+> pPrintPrec lvl 0 dcon0 $$
      vcat (map (\dcon -> "and " <+> pPrintPrec lvl 0 dcon) dcons)
    DSign s -> "val" <+> pretty s
    DClss (MkClssDecl _ c v ms) ->
      "class" <+> pretty c <+> pretty v <+> colon $$ prettyBlock lvl ms
    DInst (MkInstDecl _ c t0 qvs ds) ->
      "instance" <+> pretty c <+> pPrintPrec lvl 0 t1 <+> prettyTypeCstr qvs
      $$ prettyBlock lvl ds
      where
        t1 :: Type (TFinScope _ Void)
        t1 = mkTApp (TCon t0) (imap (\i (MkQVar _ v) -> TVar (mkBound i v)) qvs)
    DDefn d -> "let" <+> pretty d
    DSupC (MkSupCDecl _ z qvs t bs e) ->
      "let" <+>
      hang (pretty z <+> colon <+> pPrintPrecType lvl 0 (mkTUni qvs t) <+> equals) 2
        (prettyETyAbs lvl 0 qvs (prettyELam lvl 0 bs e))
    DPrim (MkPrimDecl b s) ->
      hsep ["external", pretty b, equals, text (show s)]

prettyBlock :: (Foldable t, Pretty a) => PrettyLevel -> t a -> Doc
prettyBlock lvl xs = case map (pPrintPrec lvl 0) (toList xs) of
  []     -> "{}"
  d0:ds -> "{" <+> d0 $+$ vcat (map (\d -> "," <+> d) ds) $+$ "}"

instance (BaseTVar tv) => Pretty (SignDecl tv) where
  pPrintPrec lvl _ (MkSignDecl _ x t) = pretty x <+> colon <+> pPrintPrec lvl 0 t

instance (BaseEVar ev, BaseTVar tv, PrettyStage st) => Pretty (Defn st tv ev) where
  pPrintPrec lvl _ (MkDefn b t) =
    hang (pPrintPrec lvl 0 b <+> equals) 2 (pPrintPrec lvl 0 t)

prettyDefns ::
  (BaseEVar ev, BaseTVar tv, PrettyStage st) => Bool -> Vector n (Defn st tv ev) -> Doc
prettyDefns isrec ds = case toList ds of
    [] -> mempty
    d0:ds -> vcat ((let_ <+> pretty d0) : map (\d -> "and" <+> pretty d) ds)
    where
      let_ | isrec     = "let rec"
           | otherwise = "let"

instance (BaseEVar ev, BaseTVar tv, PrettyStage st) => Pretty (Expr st tv ev) where
  pPrintPrec lvl prec = \case
    EVar _ x -> pretty (baseEVar x)
    EVal _ z -> pretty z
    ECon _ c -> pretty c
    ENum _ n -> int n
    EApp _ t us ->
      maybeParens lvl (prec > Op.aprec)
      $ pPrintPrec lvl Op.aprec t <+> hsepMap (pPrintPrec lvl (Op.aprec+1)) us
    -- This could be brought back in @EApp@ when @t@ is an operator.
    -- ApOp   { _op, _arg1, _arg2 } ->
    --   let MkSpec { _sym, _prec, _assoc } = Operator.findByName _op
    --       (prec1, prec2) =
    --         case _assoc of
    --           AssocLeft  -> (_prec  , _prec+1)
    --           AssocRight -> (_prec+1, _prec  )
    --           AssocNone  -> (_prec+1, _prec+1)
    --   in  maybeParens (prec > _prec) $
    --         pPrintPrec lvl prec1 _arg1 <> text _sym <> pPrintPrec lvl prec2 _arg2
    ELet _ ds t -> sep [prettyDefns False ds, "in"] $$ pPrintPrec lvl 0 t
    ERec _ ds t -> sep [prettyDefns True  ds, "in"] $$ pPrintPrec lvl 0 t
    ELam _ bs e _t -> prettyELam lvl prec bs e
    -- If { _cond, _then, _else } ->
    --   maybeParens (prec > 0) $ sep
    --     [ "if"  <+> pPrintPrec lvl 0 _cond <+> "then"
    --     , nest 2 (pPrintPrec lvl 0 _then)
    --     , "else"
    --     , nest 2 (pPrintPrec lvl 0 _else)
    --     ]
    EMat _ t as ->
      maybeParens lvl (prec > 0) $ vcat
      $ ("match" <+> pPrintPrec lvl 0 t <+> "with") : map (pPrintPrec lvl 0) (toList as)
    ECas _ t cs ->
      maybeParens lvl (prec > 0) $ vcat
      $ ("match" <+> pPrintPrec lvl 0 t <+> "with") : map (pPrintPrec lvl 0) (toList cs)
    ETyAbs _ vs e -> prettyETyAbs lvl prec vs (pPrintPrec lvl 0 e)
    ETyApp _ e0 ts ->
      maybeParens lvl (prec > Op.aprec)
      $ pPrintPrec lvl Op.aprec e0 <+> prettyAtType (pPrintPrecType lvl 3) ts

prettyELam ::
  (PrettyStage st, BaseTVar tv, BaseEVar ev) =>
  PrettyLevel -> Rational ->
  Vector n (Bind (StageType st) tv) -> Expr st tv (EFinScope n ev) -> Doc
prettyELam lvl prec bs e
  | null bs   = pPrintPrec lvl prec e
  | otherwise =
      maybeParens lvl (prec > 0)
      $ hang ("fun" <+> hsepMap (pPrintPrec lvl 1) bs <+> "->") 2 (pPrintPrec lvl 0 e)

prettyETyAbs :: PrettyLevel -> Rational -> Vector m QVar -> Doc -> Doc
prettyETyAbs lvl prec qvs d
  | null qvs   = maybeParens lvl (prec > 0) d
  | otherwise =
      maybeParens lvl (prec > 0)
      $ hang ("fun" <+> prettyAtType prettyQVar qvs <+> "->") 2 d

prettyAtType :: Foldable t => (a -> Doc) -> t a -> Doc
prettyAtType p = hsep . map (\x -> "@" <> p x) . toList

instance (BaseTVar tv, PrettyType ty) => Pretty (Bind ty tv) where
  pPrintPrec lvl prec (MkBind _ z t)
    | isEmpty td = zd
    | otherwise  = maybeParens lvl (prec > 0) (zd <+> colon <+> td)
    where
      zd = pretty z
      td = pPrintPrecType lvl 0 t

instance (BaseEVar ev, BaseTVar tv, PrettyStage st) => Pretty (Case st tv ev) where
  pPrintPrec lvl _ (MkCase _ c ts bs e) =
    hang
      ( "|"
        <+> pretty c
        <+> prettyAtType (pPrintPrecType lvl 3) ts
        <+> hsepMap pretty bs <+> "->"
      )
      2 (pPrintPrec lvl 0 e)

instance (BaseEVar ev, BaseTVar tv, PrettyStage st) => Pretty (Altn st tv ev) where
  pPrintPrec lvl _ (MkAltn _ p t) =
    hang ("|" <+> pPrintPrec lvl 0 p <+> "->") 2 (pPrintPrec lvl 0 t)

instance (BaseTVar tv, PrettyType ty) => Pretty (Patn ty tv) where
  pPrintPrec lvl prec = \case
    PWld _      -> "_"
    PVar _ x    -> pretty x
    PCon _ c ts ps ->
      maybeParens lvl (prec > 0 && (not (null ts) || not (null ps)))
      $ pretty c
        <+> prettyAtType (pPrintPrecType lvl 3) ts
        <+> hsep (map (pPrintPrec lvl 1) ps)

deriving instance (StageType st ~ Type) => Show (Decl st)
deriving instance (Show tv)             => Show (SignDecl tv)
deriving instance                          Show (ClssDecl)
deriving instance (StageType st ~ Type) => Show (InstDecl st)
deriving instance (StageType st ~ Type) => Show (SupCDecl st m n)
deriving instance                          Show (PrimDecl Type)
deriving instance (StageType st ~ Type, Show tv, Show ev) => Show (Defn st tv ev)
deriving instance (StageType st ~ Type, Show tv, Show ev) => Show (Expr st tv ev)
deriving instance (StageType st ~ Type, Show tv, Show ev) => Show (Case st tv ev)
deriving instance (StageType st ~ Type, Show tv, Show ev) => Show (Altn st tv ev)
deriving instance (                     Show tv) => Show (Patn Type tv)
deriving instance (                     Show tv) => Show (Bind Type tv)
