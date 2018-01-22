{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
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


  , Pos
  , module Pukeko.AST.Scope
  )
  where

import Pukeko.Prelude

import Data.Bifoldable
import Data.Bitraversable
import qualified Data.Finite as Fin

import           Pukeko.Pretty
import qualified Pukeko.AST.Operator   as Op
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Type
import           Pukeko.AST.Stage
import           Pukeko.AST.Scope
import           Pukeko.AST.ConDecl

data Module st = MkModule
  { _module2decls :: [Loc (Decl st)]
  }

data Decl st
  =                          DType (NonEmpty (Loc (Some1 TConDecl)))
  | Untyped    st ~ 'True => DSign (SignDecl Void)
  | HasClasses st ~ 'True => DClss ClssDecl
  | HasClasses st ~ 'True => DInst (InstDecl st)
  | HasLambda  st ~ 'True => DDefn (Defn st Void Void)
  | forall m n. (HasLambda st ~ 'False, StageType st ~ Type, KnownNat m) =>
                             DSupC (SupCDecl st m n)
  |                          DPrim (PrimDecl (StageType st))

data SignDecl tv = MkSignDecl
  { _sign2func :: Id.EVar
  , _sign2type :: Type tv
  }

data ClssDecl = MkClssDecl
  { _clss2name  :: Id.Clss
  , _clss2prm   :: Id.TVar
  , _clss2mthds :: [SignDecl (TFinScope 1 Void)]
  }

data InstDecl st = forall m. (KnownNat m) => MkInstDecl
  { _inst2clss  :: Id.Clss
    -- FIXME: There's no way to define instances on (->).
  , _inst2tcon  :: Id.TCon
  , _inst2qvars :: Vector m QVar
  , _inst2defns :: [Loc (Defn st (TFinScope m Void) Void)]
  }

data SupCDecl st m n = MkSupCDecl
  { _supc2func  :: Id.EVar
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
  = ELoc (Loc (Expr st tv ev))
  | EVar ev
  | EVal Id.EVar
  | ECon Id.DCon
  | ENum Int
  | EApp (Expr st tv ev) (NonEmpty (Expr st tv ev))
  | forall n. HasLambda st ~ 'True =>
    ELam (Vector n (Bind (StageType st) tv)) (Expr st tv (EFinScope n ev)) (StageType st tv)
  | forall n.
    ELet (Vector n (Loc (Defn st tv ev))) (Expr st tv (EFinScope n ev))
  | forall n.
    ERec (Vector n (Loc (Defn st tv (EFinScope n ev)))) (Expr st tv (EFinScope n ev))
  | HasNested st ~ 'False =>
    ECas (Expr st tv ev) (NonEmpty (Case st tv ev))
  | HasNested st ~ 'True =>
    EMat (Expr st tv ev) (NonEmpty (Altn st tv ev))
  | forall m. (HasTypes st ~ 'True, KnownNat m) =>
    ETyAbs (Vector m QVar) (Expr st (TFinScope m tv) ev)
  | HasTypes st ~ 'True =>
    ETyApp (Expr st tv ev) (NonEmpty (StageType st tv))

data Bind ty tv = MkBind
  { _bind2evar :: Id.EVar
  , _bind2type :: ty tv
  }

data Case st tv ev = forall n. MkCase
  { _case2dcon  :: Id.DCon
  , _case2targs :: [StageType st tv]
  , _case2binds :: Vector n Id.EVar
  , _case2expr  :: Expr st tv (EFinScope n ev)
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

mkEApp :: (Foldable t) => Expr st tv ev -> t (Expr st tv ev) -> Expr st tv ev
mkEApp e0 es0 = case toList es0 of
  []    -> e0
  e1:es -> EApp e0 (e1 :| es)

mkELam ::
  (HasLambda st ~ 'True) =>
  Vector n (Bind (StageType st) tv) ->
  Expr st tv (EFinScope n ev)       ->
  StageType st tv                   ->
  Expr st tv ev
mkELam bs e0 t0
  | null bs   = fmap unsafeStrengthen e0
  | otherwise = ELam bs e0 t0

mkETyApp ::
  (HasTypes st ~ 'True) => Expr st tv ev -> [StageType st tv] -> Expr st tv ev
mkETyApp e0 = \case
  []    -> e0
  t1:ts -> ETyApp e0 (t1 :| ts)

mkETyAbs ::
  forall st m tv ev. (IsStage st, HasTypes st ~ 'True, KnownNat m) =>
  Vector m QVar -> Expr st (TFinScope m tv) ev -> Expr st tv ev
mkETyAbs qvs e0 =
  case sameNat (Proxy @m) (Proxy @0) of
    Nothing -> ETyAbs qvs e0
    Just Refl -> first (strengthenWith Fin.absurd0) e0

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
  ETyAbs _ _   -> error "THIS IS NOT IMPLEMENTED"
  ETyApp e t   -> ETyApp (e // f) t

(///) ::
  Expr st tv (EScope i ev1) -> (ev1 -> Expr st tv ev2) -> Expr st tv (EScope i ev2)
t /// f = t // (\x -> pdist (fmap f x))

pdist :: EScope i (Expr st tv ev) -> Expr st tv (EScope i ev)
pdist (Bound i x) = EVar (Bound i x)
pdist (Free t)    = fmap Free t

-- * Lenses and traversals
inst2defn ::
  (MonadHere f) =>
  (forall n. Defn st1 (TFinScope n Void) Void -> f (Defn st2 (TFinScope n Void) Void)) ->
  InstDecl st1 -> f (InstDecl st2)
inst2defn f (MkInstDecl c t qs ds) = MkInstDecl c t qs <$> traverseHeres f ds

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
  (MonadHere f) =>
  (forall tv ev. Expr st tv ev -> f (Expr st tv ev)) -> Decl st -> f (Decl st)
decl2expr f top = case top of
  DType{} -> pure top
  DSign{} -> pure top
  DClss{} -> pure top
  DInst i -> DInst <$> inst2defn (defn2expr f) i
  DDefn d -> DDefn <$> defn2expr f d
  DSupC s -> DSupC <$> supc2expr f s
  DPrim p -> pure (DPrim p)

decl2eval :: HereTraversal' (Decl st) Id.EVar
decl2eval f = decl2expr (expr2eval f)

defn2func :: Lens' (Defn st tv ev) Id.EVar
defn2func = defn2bind . bind2evar

defn2exprSt ::
  (StageType st1 ~ StageType st2) =>
  Lens (Defn st1 tv ev1) (Defn st2 tv ev2) (Expr st1 tv ev1) (Expr st2 tv ev2)
defn2exprSt f (MkDefn b e) = MkDefn b <$> f e


-- * Deep traversals
defn2dcon :: HereTraversal' (Defn st tv ev) Id.DCon
defn2dcon f (MkDefn b e) = MkDefn b <$> expr2dcon f e

expr2dcon :: HereTraversal' (Expr st tv ev) Id.DCon
expr2dcon f = \case
  ELoc l       -> ELoc <$> traverseHere (expr2dcon f) l
  EVar x       -> pure (EVar x)
  EVal z       -> pure (EVal z)
  ECon c       -> ECon <$> f c
  ENum n       -> pure (ENum n)
  EApp t  us   -> EApp <$> expr2dcon f t <*> (traverse . expr2dcon) f us
  ECas t  cs   -> ECas <$> expr2dcon f t <*> (traverse . case2dcon) f cs
  ELam bs e t  -> ELam bs <$> expr2dcon f e <*> pure t
  ELet ds t    -> ELet <$> (traverseHeres . defn2dcon) f ds <*> expr2dcon f t
  ERec ds t    -> ERec <$> (traverseHeres . defn2dcon) f ds <*> expr2dcon f t
  EMat t  as   -> EMat <$> expr2dcon f t <*> (traverse . altn2dcon) f as
  ETyAbs x e   -> ETyAbs x <$> expr2dcon f e
  ETyApp e t   -> ETyApp <$> expr2dcon f e <*> pure t

defn2type ::
  forall m st1 st2 tv ev. (MonadHere m, SameNodes st1 st2) =>
  (forall tv. StageType st1 tv -> m (StageType st2 tv)) ->
  Defn st1 tv ev -> m (Defn st2 tv ev)
defn2type f (MkDefn b e) = MkDefn <$> bind2type f b <*> expr2type f e

expr2type ::
  forall m st1 st2 tv ev. (MonadHere m, SameNodes st1 st2) =>
  (forall tv. StageType st1 tv -> m (StageType st2 tv)) ->
  Expr st1 tv ev -> m (Expr st2 tv ev)
expr2type f = \case
  ELoc l       -> ELoc <$> traverse (expr2type f) l
  EVar x       -> pure (EVar x)
  EVal z       -> pure (EVal z)
  ECon c       -> pure (ECon c)
  ENum n       -> pure (ENum n)
  EApp e0 es   -> EApp <$> expr2type f e0 <*> traverse (expr2type f) es
  ECas e0 cs   -> ECas <$> expr2type f e0 <*> traverse (case2type f ) cs
  ELam bs e t  -> ELam <$> traverse (bind2type f) bs <*> expr2type f e <*> f t
  ELet ds e0   -> ELet <$> traverseHeres (defn2type f) ds <*> expr2type f e0
  ERec ds e0   -> ERec <$> traverseHeres (defn2type f) ds <*> expr2type f e0
  EMat e0 as   -> EMat <$> expr2type f e0 <*> traverse (altn2type f) as
  ETyAbs vs e0 -> ETyAbs vs <$> expr2type f e0
  ETyApp e0 ts -> ETyApp <$> expr2type f e0 <*> traverse f ts

case2type ::
  forall m st1 st2 tv ev. (MonadHere m, SameNodes st1 st2) =>
  (forall tv. StageType st1 tv -> m (StageType st2 tv)) ->
  Case st1 tv ev -> m (Case st2 tv ev)
case2type f (MkCase dcon targs bnds e0) =
  MkCase dcon <$> traverse f targs <*> pure bnds <*> expr2type f e0

altn2type ::
  forall m st1 st2 tv ev. (MonadHere m, SameNodes st1 st2) =>
  (forall tv. StageType st1 tv -> m (StageType st2 tv)) ->
  Altn st1 tv ev -> m (Altn st2 tv ev)
altn2type f (MkAltn patn e0) = MkAltn <$> patn2type f patn <*> expr2type f e0

patn2type ::
  forall f ty1 ty2 tv. (Applicative f) =>
  (forall tv. ty1 tv -> f (ty2 tv)) -> Patn ty1 tv -> f (Patn ty2 tv)
patn2type f = \case
  PWld -> pure PWld
  PVar x -> pure (PVar x)
  PCon dcon targs patns ->
    PCon dcon <$> traverse f targs <*> traverse (patn2type f) patns

altn2dcon :: HereTraversal' (Altn st tv ev) Id.DCon
altn2dcon f (MkAltn p t) = MkAltn <$> patn2dcon f p <*> expr2dcon f t

patn2dcon :: Traversal (Patn ty tv) (Patn ty tv) Id.DCon Id.DCon
patn2dcon f = \case
  PWld      -> pure PWld
  PVar x    -> pure (PVar x)
  PCon c ts ps -> PCon <$> f c <*> pure ts <*> (traverse . patn2dcon) f ps

expr2eval :: HereTraversal' (Expr st tv ev) Id.EVar
expr2eval f = \case
  ELoc l      -> ELoc <$> traverse (expr2eval f) l
  EVar x      -> pure (EVar x)
  EVal z      -> EVal <$> f z
  ECon c      -> pure (ECon c)
  ENum n      -> pure (ENum n)
  EApp t  us  -> EApp <$> expr2eval f t <*> (traverse . expr2eval) f us
  ELam bs e t -> ELam bs <$> expr2eval f e <*> pure t
  ELet ds t   -> ELet <$> (traverseHeres . defn2expr . expr2eval) f ds <*> expr2eval f t
  ERec ds t   -> ERec <$> (traverseHeres . defn2expr . expr2eval) f ds <*> expr2eval f t
  EMat t  as  -> EMat <$> expr2eval f t <*> (traverse . altn2expr . expr2eval) f as
  ECas t  cs  -> ECas <$> expr2eval f t <*> traverse (case2expr (expr2eval f)) cs
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

case2expr :: EScopedLens (Case st tv) (Case st tv) (Expr st tv) (Expr st tv)
case2expr f (MkCase c ts bs e) = MkCase c ts bs <$> f e


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
class PrettyType f where
  pPrintPrecType :: (BaseTVar tv) => PrettyLevel -> Rational -> f tv -> Doc

instance PrettyType NoType where
  pPrintPrecType _ _ NoType = mempty

instance PrettyType Type where
  pPrintPrecType = pPrintPrec

type PrettyStage st = PrettyType (StageType st)

instance (PrettyStage st) => Pretty (Module st) where
  pPrintPrec lvl prec (MkModule decls) = vcat (map (pPrintPrec lvl prec) decls)

instance (PrettyStage st) => Pretty (Decl st) where
  pPrintPrec lvl _ = \case
    DType (dcon0 :| dcons) ->
      "type" <+> pPrintPrec lvl 0 dcon0 $$
      vcat (map (\dcon -> "and " <+> pPrintPrec lvl 0 dcon) dcons)
    DSign s -> "val" <+> pretty s
    DClss (MkClssDecl c v ms) ->
      "class" <+> pretty c <+> pretty v <+> colon $$ prettyBlock lvl ms
    DInst (MkInstDecl c t0 qvs ds) ->
      "instance" <+> pretty c <+> pPrintPrec lvl 3 t1 <+> prettyTypeCstr qvs
      $$ prettyBlock lvl ds
      where
        t1 :: Type (TFinScope _ Void)
        t1 = mkTApp (TCon t0) (imap (\i (MkQVar _ v) -> TVar (mkBound i v)) qvs)
    DDefn d -> "let" <+> pretty d
    DSupC (MkSupCDecl z qvs t bs e) ->
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
  pPrintPrec lvl _ (MkSignDecl x t) = pretty x <+> colon <+> pPrintPrec lvl 0 t

instance (BaseEVar ev, BaseTVar tv, PrettyStage st) => Pretty (Defn st tv ev) where
  pPrintPrec lvl _ (MkDefn b t) =
    hang (pPrintPrec lvl 0 b <+> equals) 2 (pPrintPrec lvl 0 t)

prettyDefns ::
  (BaseEVar ev, BaseTVar tv, PrettyStage st) =>
  Bool -> Vector n (Loc (Defn st tv ev)) -> Doc
prettyDefns isrec ds = case toList ds of
    [] -> mempty
    d0:ds -> vcat ((let_ <+> pretty d0) : map (\d -> "and" <+> pretty d) ds)
    where
      let_ | isrec     = "let rec"
           | otherwise = "let"

instance (BaseEVar ev, BaseTVar tv, PrettyStage st) => Pretty (Expr st tv ev) where
  pPrintPrec lvl prec = \case
    ELoc l -> pPrintPrec lvl prec l
    EVar x -> pretty (baseEVar x)
    EVal z -> pretty z
    ECon c -> pretty c
    ENum n -> int n
    EApp t us ->
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
    ELet ds t -> sep [prettyDefns False ds, "in"] $$ pPrintPrec lvl 0 t
    ERec ds t -> sep [prettyDefns True  ds, "in"] $$ pPrintPrec lvl 0 t
    ELam bs e t -> prettyELamT lvl prec bs e t
    -- If { _cond, _then, _else } ->
    --   maybeParens (prec > 0) $ sep
    --     [ "if"  <+> pPrintPrec lvl 0 _cond <+> "then"
    --     , nest 2 (pPrintPrec lvl 0 _then)
    --     , "else"
    --     , nest 2 (pPrintPrec lvl 0 _else)
    --     ]
    EMat t as ->
      maybeParens lvl (prec > 0) $ vcat
      $ ("match" <+> pPrintPrec lvl 0 t <+> "with") : map (pPrintPrec lvl 0) (toList as)
    ECas t cs ->
      maybeParens lvl (prec > 0) $ vcat
      $ ("match" <+> pPrintPrec lvl 0 t <+> "with") : map (pPrintPrec lvl 0) (toList cs)
    ETyAbs vs e -> prettyETyAbs lvl prec vs (pPrintPrec lvl 0 e)
    ETyApp e0 ts ->
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

prettyELamT ::
  (PrettyStage st, BaseTVar tv, BaseEVar ev) =>
  PrettyLevel -> Rational ->
  Vector n (Bind (StageType st) tv) -> Expr st tv (EFinScope n ev) -> StageType st tv -> Doc
prettyELamT lvl prec bs e t
  | null bs   = bug "empty lambda" -- pPrintPrec lvl prec e
  | otherwise =
      maybeParens lvl (prec > 0)
      $ hang ("fun" <+> hsepMap (pPrintPrec lvl 1) bs <+> colon <+> pPrintPrecType lvl 3 t <+> "->") 2 (pPrintPrec lvl 0 e)

prettyETyAbs :: PrettyLevel -> Rational -> Vector m QVar -> Doc -> Doc
prettyETyAbs lvl prec qvs d
  | null qvs   = maybeParens lvl (prec > 0) d
  | otherwise =
      maybeParens lvl (prec > 0)
      $ hang ("fun" <+> prettyAtType prettyQVar qvs <+> "->") 2 d

prettyAtType :: Foldable t => (a -> Doc) -> t a -> Doc
prettyAtType p = hsep . map (\x -> "@" <> p x) . toList

instance (BaseTVar tv, PrettyType ty) => Pretty (Bind ty tv) where
  pPrintPrec lvl prec (MkBind z t)
    | isEmpty td = zd
    | otherwise  = maybeParens lvl (prec > 0) (zd <+> colon <+> td)
    where
      zd = pretty z
      td = pPrintPrecType lvl 0 t

instance (BaseEVar ev, BaseTVar tv, PrettyStage st) => Pretty (Case st tv ev) where
  pPrintPrec lvl _ (MkCase c ts bs e) =
    hang
      ( "|"
        <+> pretty c
        <+> prettyAtType (pPrintPrecType lvl 3) ts
        <+> hsepMap pretty bs <+> "->"
      )
      2 (pPrintPrec lvl 0 e)

instance (BaseEVar ev, BaseTVar tv, PrettyStage st) => Pretty (Altn st tv ev) where
  pPrintPrec lvl _ (MkAltn p t) =
    hang ("|" <+> pPrintPrec lvl 0 p <+> "->") 2 (pPrintPrec lvl 0 t)

instance (BaseTVar tv, PrettyType ty) => Pretty (Patn ty tv) where
  pPrintPrec lvl prec = \case
    PWld -> "_"
    PVar x    -> pretty x
    PCon c ts ps ->
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
