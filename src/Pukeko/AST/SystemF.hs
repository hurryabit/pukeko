{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

  , weakenE
  , mkEApp
  , mkELam
  , mkETyApp
  , mkETyAbs

  , abstract
  , (//)

  , module2decls
  , decl2func
  , sign2func
  , sign2type
  , inst2defn
  , prim2bind
  , defn2bind
  , defn2func
  , defn2expr
  , defn2exprSt
  , defn2dcon
  , defn2type
  , bind2evar
  , bind2type

  , decl2eval
  , expr2eval
  , patn2evar

  , module Pukeko.AST.Scope
  )
  where

import Pukeko.Prelude

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
  { _module2decls :: [Lctd (Decl st)]
  }

data Decl st
  =                          DType (NonEmpty (Lctd TConDecl))
  | Untyped    st ~ 'True => DSign (SignDecl Void)
  | HasClasses st ~ 'True => DClss ClssDecl
  | HasClasses st ~ 'True => DInst (InstDecl st)
  | HasLambda  st ~ 'True => DDefn (Defn st Void Void)
  | (HasLambda st ~ 'False, StageType st ~ Type) =>
                             DSupC (SupCDecl st)
  |                          DPrim (PrimDecl (StageType st))

data SignDecl tv = MkSignDecl
  { _sign2func :: Id.EVar
  , _sign2type :: Type tv
  }

data ClssDecl = MkClssDecl
  { _clss2name  :: Id.Clss
  , _clss2prm   :: Id.TVar
  , _clss2mthds :: [SignDecl (TScope Int Void)]
  }

data InstDecl st = MkInstDecl
  { _inst2clss  :: Id.Clss
    -- FIXME: There's no way to define instances on (->).
  , _inst2tcon  :: Id.TCon
  , _inst2qvars :: [QVar]
  , _inst2defns :: [Lctd (Defn st (TScope Int Void) Void)]
  }

data SupCDecl st = MkSupCDecl
  { _supc2func  :: Id.EVar
  , _supc2tprms :: [QVar]
  , _supc2type  :: StageType st (TScope Int Void)
  , _supc2eprms :: [Bind (StageType st) (TScope Int Void)]
  , _supc2expr  :: Expr st (TScope Int Void) (EScope Int Void)
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
  = ELoc (Lctd (Expr st tv ev))
  | EVar ev
  | EVal Id.EVar
  | ECon Id.DCon
  | ENum Int
  | EApp (Expr st tv ev) (NonEmpty (Expr st tv ev))
  | HasLambda st ~ 'True =>
    ELam (NonEmpty (Bind (StageType st) tv)) (Expr st tv (EScope Int ev)) (StageType st tv)
  | ELet [Lctd (Defn st tv ev)] (Expr st tv (EScope Int ev))
  | ERec [Lctd (Defn st tv (EScope Int ev))] (Expr st tv (EScope Int ev))
  | HasNested st ~ 'False =>
    ECas (Expr st tv ev) (NonEmpty (Case st tv ev))
  | HasNested st ~ 'True =>
    EMat (Expr st tv ev) (NonEmpty (Altn st tv ev))
  | ECoe (Coercion (StageType st tv)) (Expr st tv ev)
  | (HasTypes st ~ 'True) =>
    ETyAbs (NonEmpty QVar) (Expr st (TScope Int tv) ev)
  | HasTypes st ~ 'True =>
    ETyApp (Expr st tv ev) (NonEmpty (StageType st tv))

data Bind ty tv = MkBind
  { _bind2evar :: Id.EVar
  , _bind2type :: ty tv
  }

data Case st tv ev = MkCase
  { _case2dcon  :: Id.DCon
  , _case2targs :: [StageType st tv]
  , _case2binds :: [Maybe Id.EVar]
  , _case2expr  :: Expr st tv (EScope Int ev)
  }

data Altn st tv ev = MkAltn
  { _altn2patn :: Patn (StageType st) tv
  , _altn2expr :: Expr st tv (EScope Id.EVar ev)
  }

data Patn ty tv
  = PWld
  | PVar Id.EVar
  | PCon Id.DCon [ty tv] [Patn ty tv]

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

weakenE :: Expr st tv ev -> Expr st tv (EScope i ev)
weakenE = fmap weakenScope

strengthenE0 :: Expr st tv (EScope Int ev) -> Expr st tv ev
strengthenE0 = fmap strengthenScope0

mkEApp :: (Foldable t) => Expr st tv ev -> t (Expr st tv ev) -> Expr st tv ev
mkEApp e0 es0 = case toList es0 of
  []    -> e0
  e1:es -> EApp e0 (e1 :| es)

mkELam ::
  (HasLambda st ~ 'True) =>
  [Bind (StageType st) tv]   ->
  Expr st tv (EScope Int ev) ->
  StageType st tv            ->
  Expr st tv ev
mkELam bs0 e0 t0 = case bs0 of
  []   -> strengthenE0 e0
  b:bs -> ELam (b :| bs) e0 t0

mkETyApp ::
  (HasTypes st ~ 'True) => Expr st tv ev -> [StageType st tv] -> Expr st tv ev
mkETyApp e0 = \case
  []    -> e0
  t1:ts -> ETyApp e0 (t1 :| ts)

mkETyAbs :: (IsStage st, HasTypes st ~ 'True) =>
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

-- | Replace subexpressions.
-- FIXME: Make this a proper monad instance.
(//) :: Expr st tv ev1 -> (ev1 -> Expr st tv ev2) -> Expr st tv ev2
expr // f = case expr of
  ELoc l       -> ELoc (fmap (// f) l)
  EVar x       -> f x
  EVal z       -> EVal z
  ECon c       -> ECon c
  ENum n       -> ENum n
  EApp t  us   -> EApp (t // f) (fmap (// f) us)
  ECas t  cs   -> ECas (t // f) (fmap (over' case2expr (/// f)) cs)
  ELam ps e t  -> ELam ps (e /// f) t
  ELet ds t    -> ELet (over (traverse . traverse . defn2expr) (//  f) ds) (t /// f)
  ERec ds t    -> ERec (over (traverse . traverse . defn2expr) (/// f) ds) (t /// f)
  EMat t  as   -> EMat (t // f) (fmap (over altn2expr (/// f)) as)
  ECoe c e     -> ECoe c (e // f)
  ETyAbs _ _   -> error "THIS IS NOT IMPLEMENTED"
  ETyApp e t   -> ETyApp (e // f) t

(///) ::
  Expr st tv (EScope i ev1) -> (ev1 -> Expr st tv ev2) -> Expr st tv (EScope i ev2)
t /// f = t // (\x -> pdist (fmap f x))

pdist :: EScope i (Expr st tv ev) -> Expr st tv (EScope i ev)
pdist (Bound i x) = EVar (Bound i x)
pdist (Free t)    = fmap Free t

-- * Lenses and traversals
inst2defn :: WhereTraversal
  (InstDecl st1) (InstDecl st2)
  (Defn st1 (TScope Int Void) Void) (Defn st2 (TScope Int Void) Void)
inst2defn f (MkInstDecl c t qs ds) = MkInstDecl c t qs <$> (traverse . lctd) f ds

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
  (Applicative f, Where f) =>
  (forall tv ev. Expr st tv ev -> f (Expr st tv ev)) -> Decl st -> f (Decl st)
decl2expr f top = case top of
  DType{} -> pure top
  DSign{} -> pure top
  DClss{} -> pure top
  DInst i -> DInst <$> inst2defn (defn2expr f) i
  DDefn d -> DDefn <$> defn2expr f d
  DSupC s -> DSupC <$> supc2expr f s
  DPrim p -> pure (DPrim p)

decl2eval :: WhereTraversal' (Decl st) Id.EVar
decl2eval f = decl2expr (expr2eval f)

defn2func :: Lens' (Defn st tv ev) Id.EVar
defn2func = defn2bind . bind2evar

defn2exprSt ::
  (StageType st1 ~ StageType st2) =>
  Lens (Defn st1 tv ev1) (Defn st2 tv ev2) (Expr st1 tv ev1) (Expr st2 tv ev2)
defn2exprSt f (MkDefn b e) = MkDefn b <$> f e


-- * Deep traversals
defn2dcon :: WhereTraversal' (Defn st tv ev) Id.DCon
defn2dcon f (MkDefn b e) = MkDefn b <$> expr2dcon f e

expr2dcon :: WhereTraversal' (Expr st tv ev) Id.DCon
expr2dcon f = \case
  ELoc l       -> ELoc <$> lctd (expr2dcon f) l
  EVar x       -> pure (EVar x)
  EVal z       -> pure (EVal z)
  ECon c       -> ECon <$> f c
  ENum n       -> pure (ENum n)
  EApp t  us   -> EApp <$> expr2dcon f t <*> (traverse . expr2dcon) f us
  ECas t  cs   -> ECas <$> expr2dcon f t <*> (traverse . case2dcon) f cs
  ELam bs e t  -> ELam bs <$> expr2dcon f e <*> pure t
  ELet ds t    -> ELet <$> (traverse . lctd . defn2dcon) f ds <*> expr2dcon f t
  ERec ds t    -> ERec <$> (traverse . lctd . defn2dcon) f ds <*> expr2dcon f t
  EMat t  as   -> EMat <$> expr2dcon f t <*> (traverse . altn2dcon) f as
  ECoe d e     -> ECoe d <$> expr2dcon f e
  ETyAbs x e   -> ETyAbs x <$> expr2dcon f e
  ETyApp e t   -> ETyApp <$> expr2dcon f e <*> pure t

defn2type ::
  forall f st1 st2 tv ev. (Applicative f, Where f, SameNodes st1 st2) =>
  (forall tv. StageType st1 tv -> f (StageType st2 tv)) ->
  Defn st1 tv ev -> f (Defn st2 tv ev)
defn2type f (MkDefn b e) = MkDefn <$> bind2type f b <*> expr2type f e

expr2type ::
  forall f st1 st2 tv ev. (Applicative f, Where f, SameNodes st1 st2) =>
  (forall tv. StageType st1 tv -> f (StageType st2 tv)) ->
  Expr st1 tv ev -> f (Expr st2 tv ev)
expr2type f = \case
  ELoc l       -> ELoc <$> traverse (expr2type f) l
  EVar x       -> pure (EVar x)
  EVal z       -> pure (EVal z)
  ECon c       -> pure (ECon c)
  ENum n       -> pure (ENum n)
  EApp e0 es   -> EApp <$> expr2type f e0 <*> traverse (expr2type f) es
  ECas e0 cs   -> ECas <$> expr2type f e0 <*> traverse (case2type f ) cs
  ELam bs e t  -> ELam <$> traverse (bind2type f) bs <*> expr2type f e <*> f t
  ELet ds e0   -> ELet <$> (traverse . lctd) (defn2type f) ds <*> expr2type f e0
  ERec ds e0   -> ERec <$> (traverse . lctd) (defn2type f) ds <*> expr2type f e0
  EMat e0 as   -> EMat <$> expr2type f e0 <*> traverse (altn2type f) as
  ECoe c e     -> ECoe <$> coercion2type f c <*> expr2type f e
  ETyAbs vs e0 -> ETyAbs vs <$> expr2type f e0
  ETyApp e0 ts -> ETyApp <$> expr2type f e0 <*> traverse f ts

case2type ::
  forall f st1 st2 tv ev. (Applicative f, Where f, SameNodes st1 st2) =>
  (forall tv. StageType st1 tv -> f (StageType st2 tv)) ->
  Case st1 tv ev -> f (Case st2 tv ev)
case2type f (MkCase dcon targs bnds e0) =
  MkCase dcon <$> traverse f targs <*> pure bnds <*> expr2type f e0

altn2type ::
  forall f st1 st2 tv ev. (Applicative f, Where f, SameNodes st1 st2) =>
  (forall tv. StageType st1 tv -> f (StageType st2 tv)) ->
  Altn st1 tv ev -> f (Altn st2 tv ev)
altn2type f (MkAltn patn e0) = MkAltn <$> patn2type f patn <*> expr2type f e0

patn2type ::
  forall f ty1 ty2 tv. (Applicative f) =>
  (forall tv. ty1 tv -> f (ty2 tv)) -> Patn ty1 tv -> f (Patn ty2 tv)
patn2type f = \case
  PWld -> pure PWld
  PVar x -> pure (PVar x)
  PCon dcon targs patns ->
    PCon dcon <$> traverse f targs <*> traverse (patn2type f) patns

altn2dcon :: WhereTraversal' (Altn st tv ev) Id.DCon
altn2dcon f (MkAltn p t) = MkAltn <$> patn2dcon f p <*> expr2dcon f t

patn2dcon :: Traversal (Patn ty tv) (Patn ty tv) Id.DCon Id.DCon
patn2dcon f = \case
  PWld      -> pure PWld
  PVar x    -> pure (PVar x)
  PCon c ts ps -> PCon <$> f c <*> pure ts <*> (traverse . patn2dcon) f ps

expr2eval :: WhereTraversal' (Expr st tv ev) Id.EVar
expr2eval f = \case
  ELoc l      -> ELoc <$> traverse (expr2eval f) l
  EVar x      -> pure (EVar x)
  EVal z      -> EVal <$> f z
  ECon c      -> pure (ECon c)
  ENum n      -> pure (ENum n)
  EApp t  us  -> EApp <$> expr2eval f t <*> (traverse . expr2eval) f us
  ELam bs e t -> ELam bs <$> expr2eval f e <*> pure t
  ELet ds t   ->
    ELet <$> (traverse . lctd . defn2expr . expr2eval) f ds <*> expr2eval f t
  ERec ds t   ->
    ERec <$> (traverse . lctd . defn2expr . expr2eval) f ds <*> expr2eval f t
  EMat t  as  -> EMat <$> expr2eval f t <*> (traverse . altn2expr . expr2eval) f as
  ECas t  cs  -> ECas <$> expr2eval f t <*> traverse (case2expr (expr2eval f)) cs
  ECoe d  e   -> ECoe d <$> expr2eval f e
  ETyAbs x e  -> ETyAbs x <$> expr2eval f e
  ETyApp e t  -> ETyApp <$> expr2eval f e <*> pure t

patn2evar :: Traversal' (Patn st tv) Id.EVar
patn2evar f = \case
  PWld      -> pure PWld
  PVar x    -> PVar <$> f x
  PCon c ts ps -> PCon c ts <$> (traverse . patn2evar) f ps

-- * Scoped lenses
type EScopedLens s t a b =
  forall f ev1 ev2. (Functor f) =>
  (forall i. a (EScope i ev1) -> f (b (EScope i ev2))) -> s ev1 -> f (t ev2)

over' ::
  ((forall i. g (s i v1) -> Identity (g (s i v2))) -> f v1 -> Identity (f v2)) ->
   (forall i. g (s i v1) ->           g (s i v2))  -> f v1 ->           f v2
over' l f = runIdentity . l (Identity . f)


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
instance IsStage st => Bifunctor     (Defn st) where
  bimap = bimapDefault
instance IsStage st => Bifoldable    (Defn st) where
  bifoldMap = bifoldMapDefault
instance IsStage st => Bitraversable (Defn st) where
  bitraverse f g (MkDefn b e) = MkDefn <$> traverse f b <*> bitraverse f g e

instance IsStage st => Bifunctor     (Expr st) where
  bimap = bimapDefault
instance IsStage st => Bifoldable    (Expr st) where
  bifoldMap = bifoldMapDefault
instance IsStage st => Bitraversable (Expr st) where
  bitraverse f g = \case
    ELoc l -> ELoc <$> traverse (bitraverse f g) l
    EVar x -> EVar <$> g x
    EVal z -> pure (EVal z)
    ECon c -> pure (ECon c)
    ENum n -> pure (ENum n)
    EApp e0 es -> EApp <$> bitraverse f g e0 <*> traverse (bitraverse f g) es
    ELam bs e0 t ->
      ELam
      <$> traverse (traverse f) bs
      <*> bitraverse f (traverse g) e0
      <*> traverse f t
    ELet ds e0 ->
      ELet
      <$> traverse (traverse (bitraverse f g)) ds
      <*> bitraverse f (traverse g) e0
    ERec ds e0 ->
      ERec
      <$> traverse (traverse (bitraverse f (traverse g))) ds
      <*> bitraverse f (traverse g) e0
    ECas e0 cs -> ECas <$> bitraverse f g e0 <*> traverse (bitraverse f g) cs
    EMat e0 as -> EMat <$> bitraverse f g e0 <*> traverse (bitraverse f g) as
    ETyAbs v e -> ETyAbs v <$> bitraverse (traverse f) g e
    ETyApp e t -> ETyApp <$> bitraverse f g e <*> traverse (traverse f) t
    ECoe c e   -> ECoe <$> coercion2type (traverse f) c <*> bitraverse f g e

instance IsStage st => Bifunctor     (Case st) where
  bimap = bimapDefault
instance IsStage st => Bifoldable    (Case st) where
  bifoldMap = bifoldMapDefault
instance IsStage st => Bitraversable (Case st) where
  bitraverse f g (MkCase c ts bs e) =
    MkCase c
    <$> traverse (traverse f) ts
    <*> pure bs
    <*> bitraverse f (traverse g) e

instance IsStage st => Bifunctor     (Altn st) where
  bimap = bimapDefault
instance IsStage st => Bifoldable    (Altn st) where
  bifoldMap = bifoldMapDefault
instance IsStage st => Bitraversable (Altn st) where
  bitraverse f g (MkAltn p e) = MkAltn <$> traverse f p <*> bitraverse f (traverse g) e

-- * Pretty printing
class IsType ty => PrettyType ty where
  prettyPrecType :: (BaseTVar tv) => Int -> ty tv -> Doc ann

instance PrettyType NoType where
  prettyPrecType _ NoType = mempty

instance PrettyType Type where
  prettyPrecType = prettyPrec

type PrettyStage st = PrettyType (StageType st)

instance (PrettyStage st) => Pretty (Module st) where
  pretty (MkModule decls) = vcatMap pretty decls

instance (PrettyStage st) => Pretty (Decl st) where
  pretty = \case
    DType (dcon0 :| dcons) ->
      "data" <+> pretty dcon0
      $$ vcatMap (\dcon -> "and " <+> pretty dcon) dcons
    DSign s -> pretty s
    DClss (MkClssDecl c v ms) ->
      "class" <+> pretty c <+> pretty v <+> "where"
      $$ nest 2 (vcatMap pretty ms)
    DInst (MkInstDecl c t0 qvs ds) ->
      "instance" <+> prettyTypeCstr qvs <+> pretty c <+> prettyPrec 3 t1
      $$ nest 2 (vcatMap pretty ds)
      where
        t1 :: Type (TScope Int Void)
        t1 = mkTApp (TCon t0) (imap (\i (MkQVar _ v) -> TVar (mkBound i v)) qvs)
    DDefn d -> pretty d
    DSupC (MkSupCDecl z qvs t bs e) ->
      hang (pretty z <+> ":" <+> prettyPrecType 0 (mkTUni qvs t) <+> "=") 2
        (prettyETyAbs 0 qvs (prettyELam 0 bs e))
    DPrim (MkPrimDecl b s) ->
      hsep ["external", pretty b, "=", doubleQuotes (pretty s)]

instance (BaseTVar tv) => Pretty (SignDecl tv) where
  pretty (MkSignDecl x t) = pretty x <+> ":" <+> pretty t

instance (BaseEVar ev, BaseTVar tv, PrettyStage st) => Pretty (Defn st tv ev) where
  pretty (MkDefn b t) =
    hang (pretty b <+> "=") 2 (prettyPrec 0 t)

prettyDefns ::
  (BaseEVar ev, BaseTVar tv, PrettyStage st) =>
  Bool -> [Lctd (Defn st tv ev)] -> Doc ann
prettyDefns isrec ds = case ds of
    [] -> bug "empty definitions"
    d0:ds ->
      let_ <+> pretty d0
      $$ vcatMap (\d -> "and" <+> pretty d) ds
    where
      let_ | isrec     = "let rec"
           | otherwise = "let"

instance (BaseEVar ev, BaseTVar tv, PrettyStage st) => Pretty (Expr st tv ev)

instance (BaseEVar ev, BaseTVar tv, PrettyStage st) => PrettyPrec (Expr st tv ev) where
  prettyPrec prec = \case
    ELoc l -> prettyPrec prec l
    EVar x -> pretty (baseEVar x)
    EVal z -> pretty z
    ECon c -> pretty c
    ENum n -> pretty n
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
          Just (t_from, t_to) -> (prettyPrecType 1 t_from, prettyPrecType 0 t_to)
          Nothing -> case dir of
            Inject  -> ("_", pretty tcon)
            Project -> (pretty tcon, "_")
    ETyAbs vs e -> prettyETyAbs prec vs (pretty e)
    ETyApp e0 ts ->
      maybeParens (prec > Op.aprec)
      $ prettyPrec Op.aprec e0 <+> prettyAtType (prettyPrecType 3) ts

prettyELam ::
  (PrettyStage st, BaseTVar tv, BaseEVar ev) =>
  Int -> [Bind (StageType st) tv] -> Expr st tv (EScope Int ev) -> Doc ann
prettyELam prec bs e
  | null bs   = prettyPrec prec e
  | otherwise =
      maybeParens (prec > 0)
      $ hang ("fun" <+> hsepMap (prettyPrec 1) bs <+> "->") 2 (pretty e)

prettyELamT ::
  (PrettyStage st, BaseTVar tv, BaseEVar ev) =>
  Int ->
  NonEmpty (Bind (StageType st) tv) -> Expr st tv (EScope Int ev) -> StageType st tv -> Doc ann
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

deriving instance (StageType st ~ Type) => Show (Decl st)
deriving instance (Show tv)             => Show (SignDecl tv)
deriving instance                          Show (ClssDecl)
deriving instance (StageType st ~ Type) => Show (InstDecl st)
deriving instance (StageType st ~ Type) => Show (SupCDecl st)
deriving instance                          Show (PrimDecl Type)
deriving instance (StageType st ~ Type, Show tv, Show ev) => Show (Defn st tv ev)
deriving instance (StageType st ~ Type, Show tv, Show ev) => Show (Expr st tv ev)
deriving instance (StageType st ~ Type, Show tv, Show ev) => Show (Case st tv ev)
deriving instance (StageType st ~ Type, Show tv, Show ev) => Show (Altn st tv ev)
deriving instance (                     Show tv) => Show (Patn Type tv)
deriving instance (                     Show tv) => Show (Bind Type tv)
