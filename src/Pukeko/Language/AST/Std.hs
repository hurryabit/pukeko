{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.Language.AST.Std
  ( GenModuleInfo (..)
  , ModuleInfo
  , Module (..)
  , TopLevel (..)
  , Defn (..)
  , Expr (..)
  , Bind (..)
  , Case (..)
  , Altn (..)
  , Patn (..)

  , mkEApp
  , mkETyApp

  , abstract
  , (//)

  , bindEVar
  , bindType
  , defnLhs
  , caseCon
  , caseTArgs
  , altnPatn
  , altnRhs

  , module2tops
  , defn2rhs
  , defn2dcon
  , expr2eval
  , patn2evar
  , case2rhs
  , altn2rhs

  , retagDefn
  , retagExpr
  , retagBind

  , Pos
  , module Pukeko.Language.AST.Scope
  )
  where

import Control.Lens
import Data.Vector.Sized (Vector)
import Data.Foldable
import qualified Data.List.NonEmpty as NE
import           GHC.TypeLits

import           Pukeko.Pos
import           Pukeko.Pretty
import qualified Pukeko.Language.Operator as Op
import qualified Pukeko.Language.Ident    as Id
import           Pukeko.Language.Type
import           Pukeko.Language.AST.Classes
import           Pukeko.Language.AST.Stage
import           Pukeko.Language.AST.Scope
import           Pukeko.Language.AST.ModuleInfo
import qualified Pukeko.Language.AST.ConDecl as Con

type ModuleInfo st = GenModuleInfo (HasMICons st) (HasMIFuns st)

data Module st = MkModule
  { _moduleInfo :: GenModuleInfo (HasMICons st) (HasMIFuns st)
  , _moduleTops :: [TopLevel st]
  }

data TopLevel st
  = HasTLTyp st ~ 'True =>
    TLTyp Pos [Con.TConDecl]
  | HasTLVal st ~ 'True =>
    TLVal Pos Id.EVar (Type Void)
  | HasLambda st ~ 'True =>
    TLDef     (Defn st Void Void)
  | forall m n. (HasLambda st ~ 'False, KnownNat m) =>
    TLSup Pos Id.EVar
      (Vector m Id.TVar)
      (StageType st (TFinScope m Void))
      (Vector n (Bind st (TFinScope m Void)))
      (Expr st (TFinScope m Void) (EFinScope n Void))
  | TLAsm     (Bind st Void) String

data Defn st tv ev = MkDefn
  { _defnLhs :: Bind st tv
  , _defnRhs :: Expr st tv ev
  }

data Expr st tv ev
  = EVar Pos ev
  | EVal Pos Id.EVar
  | ECon Pos Id.DCon
  | ENum Pos Int
  | EApp Pos (Expr st tv ev) [Expr st tv ev]
  | forall n. HasLambda st ~ 'True =>
    ELam Pos (Vector n (Bind st tv))                  (Expr st tv (EFinScope n ev))
  | forall n.
    ELet Pos (Vector n (Defn st tv ev))               (Expr st tv (EFinScope n ev))
  | forall n.
    ERec Pos (Vector n (Defn st tv (EFinScope n ev))) (Expr st tv (EFinScope n ev))
  | HasNested st ~ 'False =>
    ECas Pos (Expr st tv ev) (NE.NonEmpty (Case st tv ev))
  | HasNested st ~ 'True =>
    EMat Pos (Expr st tv ev) (NE.NonEmpty (Altn st tv ev))
  | forall n. (HasTypes st ~ 'True, HasLambda st ~ 'True, KnownNat n) =>
    ETyAbs Pos (Vector n Id.TVar) (Expr st (TFinScope n tv) ev)
  | HasTypes st ~ 'True =>
    ETyApp Pos (Expr st tv ev) [StageType st tv]

data Bind st tv = MkBind
  { _bindPos  :: Pos
  , _bindEVar :: Id.EVar
  , _bindType :: StageType st tv
  }

data Case st tv ev = forall n. MkCase
  { _casePos   :: Pos
  , _caseCon   :: Id.DCon
  , _caseTArgs :: [StageType st tv]
  , _caseBinds :: Vector n (Maybe Id.EVar)
  , _caseRhs   :: Expr st tv (EFinScope n ev)
  }

data Altn st tv ev = MkAltn
  { _altnPos  :: Pos
  , _altnPatn :: Patn st tv
  , _altnRhs  :: Expr st tv (EScope Id.EVar ev)
  }

data Patn st tv
  = PWld Pos
  | PVar Pos Id.EVar
  | PCon Pos Id.DCon [StageType st tv] [Patn st tv]

-- * Derived optics
makeLenses ''Defn
makeLenses ''Bind
makeLenses ''Case
makeLenses ''Altn

mkEApp :: Pos -> Expr st tv ev -> [Expr st tv ev] -> Expr st tv ev
mkEApp w e0 es
  | null es   = e0
  | otherwise = EApp w e0 es

mkETyApp ::
  (HasTypes st ~ 'True) => Pos -> Expr st tv ev -> [StageType st tv] -> Expr st tv ev
mkETyApp w e0 ts
  | null ts   = e0
  | otherwise = ETyApp w e0 ts


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
  EApp w t  us   -> EApp w (t // f) (map (// f) us)
  ECas w t  cs   -> ECas w (t // f) (fmap (over' case2rhs (/// f)) cs)
  ELam w ps t    -> ELam w ps (t /// f)
  ELet w ds t    -> ELet w (over (traverse . defn2rhs) (//  f) ds) (t /// f)
  ERec w ds t    -> ERec w (over (traverse . defn2rhs) (/// f) ds) (t /// f)
  EMat w t  as   -> EMat w (t // f) (fmap (over' altn2rhs (/// f)) as)
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
module2tops ::
  SameModuleInfo st1 st2 =>
  Lens (Module st1) (Module st2) [TopLevel st1] [TopLevel st2]
module2tops f (MkModule info tops) = MkModule info <$> f tops

defn2rhs ::
  (StageType st1 ~ StageType st2) =>
  Lens (Defn st1 tv ev1) (Defn st2 tv ev2) (Expr st1 tv ev1) (Expr st2 tv ev2)
defn2rhs f (MkDefn b e) = MkDefn (retagBind b) <$> f e

-- * Deep traversals
type DConTraversal t =
  forall st1 st2 tv ev. (SameNodes st1 st2, SameTypes st1 st2) =>
  IndexedTraversal Pos (t st1 tv ev) (t st2 tv ev) Id.DCon Id.DCon

defn2dcon :: DConTraversal Defn
defn2dcon f (MkDefn b e) = MkDefn (retagBind b) <$> expr2dcon f e

expr2dcon :: DConTraversal Expr
expr2dcon f = \case
  EVar w x       -> pure (EVar w x)
  EVal w z       -> pure (EVal w z)
  ECon w c       -> ECon w <$> indexed f w c
  ENum w n       -> pure (ENum w n)
  EApp w t  us   -> EApp w <$> expr2dcon f t <*> (traverse . expr2dcon) f us
  ECas w t  cs   -> ECas w <$> expr2dcon f t <*> (traverse . case2dcon) f cs
  ELam w bs t    -> ELam w (fmap retagBind bs) <$> expr2dcon f t
  ELet w ds t    -> ELet w <$> (traverse . defn2dcon) f ds <*> expr2dcon f t
  ERec w ds t    -> ERec w <$> (traverse . defn2dcon) f ds <*> expr2dcon f t
  EMat w t  as   -> EMat w <$> expr2dcon f t <*> (traverse . altn2dcon) f as
  ETyAbs w x e   -> ETyAbs w x <$> expr2dcon f e
  ETyApp w e t   -> ETyApp w <$> expr2dcon f e <*> pure t

case2dcon :: DConTraversal Case
case2dcon f (MkCase w c ts bs e) =
  MkCase w <$> indexed f w c <*> pure ts <*> pure bs <*> expr2dcon f e

altn2dcon :: DConTraversal Altn
altn2dcon f (MkAltn w p t) =
  MkAltn w <$> patn2dcon f p <*> expr2dcon f t

patn2dcon ::
  (SameTypes st1 st2) =>
  IndexedTraversal Pos (Patn st1 tv) (Patn st2 tv) Id.DCon Id.DCon
patn2dcon f = \case
  PWld w      -> pure (PWld w)
  PVar w x    -> pure (PVar w x)
  PCon w c ts ps -> PCon w <$> indexed f w c <*> pure ts <*> (traverse . patn2dcon) f ps

type EValTraversal t =
  forall st1 st2 tv ev. (SameNodes st1 st2, SameTypes st1 st2) =>
  IndexedTraversal Pos (t st1 tv ev) (t st2 tv ev) Id.EVar Id.EVar

defn2eval :: EValTraversal Defn
defn2eval f (MkDefn b e) = MkDefn (retagBind b) <$> expr2eval f e

expr2eval :: EValTraversal Expr
expr2eval f = \case
  EVar w x       -> pure (EVar w x)
  EVal w z       -> EVal w <$> indexed f w z
  ECon w c       -> pure (ECon w c)
  ENum w n       -> pure (ENum w n)
  EApp w t  us   -> EApp w <$> expr2eval f t <*> (traverse . expr2eval) f us
  ECas w t  cs   -> ECas w <$> expr2eval f t <*> (traverse . case2eval) f cs
  ELam w bs t    -> ELam w (fmap retagBind bs) <$> expr2eval f t
  ELet w ds t    -> ELet w <$> (traverse . defn2eval) f ds <*> expr2eval f t
  ERec w ds t    -> ERec w <$> (traverse . defn2eval) f ds <*> expr2eval f t
  EMat w t  as   -> EMat w <$> expr2eval f t <*> (traverse . altn2eval) f as
  ETyAbs w x e   -> ETyAbs w x <$> expr2eval f e
  ETyApp w e t   -> ETyApp w <$> expr2eval f e <*> pure t

case2eval :: EValTraversal Case
case2eval f (MkCase w c ts bs e) =
  MkCase w c <$> pure ts <*> pure bs <*> expr2eval f e

altn2eval :: EValTraversal Altn
altn2eval f (MkAltn w p t) =
  MkAltn w (retagPatn p) <$> expr2eval f t

-- type TypeTraversal t =
--   forall st1 st2 tv ev. (SameNodes st1 st2, HasETyp st1 ~ 'False) =>
--   IndexedTraversal Pos
--   -- Traversal
--   (t st1 tv ev) (t st2 tv ev) (StageType st1 tv) (StageType st2 tv)

-- defn2type :: TypeTraversal Defn
-- defn2type f (MkDefn b e) = MkDefn <$> bind2type f b <*> expr2type f e

-- expr2type :: TypeTraversal Expr
-- expr2type f = \case
--   EVar w x -> pure (EVar w x)
--   ECon w c -> pure (ECon w c)
--   ENum w n -> pure (ENum w n)
--   EApp w e1 e2 -> EApp w <$> expr2type f e1 <*> (traverse . expr2type) f e2
--   ELam w bs e0 -> ELam w <$> (traverse . bind2type) f bs <*> expr2type f e0
--   ELet w ds e0 -> ELet w <$> (traverse . defn2type) f ds <*> expr2type f e0
--   ERec w ds e0 -> ERec w <$> (traverse . defn2type) f ds <*> expr2type f e0
--   ECas w e0 cs -> ECas w <$> expr2type f e0 <*> traverse (case2type f) cs
--   EMat w e0 as -> EMat w <$> expr2type f e0 <*> traverse (altn2rhs (expr2type f)) as

-- case2type :: TypeTraversal Case
-- case2type f (MkCase w c ts bs e) =
--   MkCase w c <$> traverse (indexed f w) ts <*> pure bs <*> pure e

-- bind2type ::
--   IndexedLens Pos (Bind st1 tv) (Bind st2 tv) (StageType st1 tv) (StageType st2 tv)
-- bind2type f (MkBind w x t) = MkBind w x <$> indexed f w t

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

case2rhs ::
  (SameTypes st1 st2) =>
  EScopedLens (Case st1 tv) (Case st2 tv) (Expr st1 tv) (Expr st2 tv)
case2rhs f (MkCase w c ts bs e) = MkCase w c ts bs <$> f e

altn2rhs ::
  (SameTypes st1 st2) =>
  EScopedLens (Altn st1 tv) (Altn st2 tv) (Expr st1 tv) (Expr st2 tv)
altn2rhs f (MkAltn w p t) = MkAltn w (retagPatn p) <$> f t

-- * Retagging
retagDefn :: (SameNodes st1 st2, SameTypes st1 st2) => Defn st1 tv ev -> Defn st2 tv ev
retagDefn = over defn2dcon id

retagExpr :: (SameNodes st1 st2, SameTypes st1 st2) => Expr st1 tv ev -> Expr st2 tv ev
retagExpr = over expr2dcon id

retagBind :: (SameTypes st1 st2) => Bind st1 tv -> Bind st2 tv
retagBind (MkBind w x t) = MkBind w x t

retagPatn :: (SameTypes st1 st2) => Patn st1 tv -> Patn st2 tv
retagPatn = over patn2dcon id


-- * Manual instances
instance FunctorWithIndex     Pos (Defn st tv) where
instance FoldableWithIndex    Pos (Defn st tv) where
instance TraversableWithIndex Pos (Defn st tv) where
  itraverse f (MkDefn b e) = MkDefn b <$> itraverse f e

instance FunctorWithIndex     Pos (Expr st tv) where
instance FoldableWithIndex    Pos (Expr st tv) where
instance TraversableWithIndex Pos (Expr st tv) where
  itraverse f = \case
    EVar w x -> EVar w <$> f w x
    EVal w z -> pure (EVal w z)
    ECon w c -> pure (ECon w c)
    ENum w n -> pure (ENum w n)
    EApp w e0 es -> EApp w <$> itraverse f e0 <*> (traverse . itraverse) f es
    ELam w bs e0 -> ELam w bs <$> itraverse (traverse . f) e0
    ELet w ds e0 ->
      ELet w <$> (traverse . itraverse) f ds <*> itraverse (traverse . f) e0
    ERec w ds e0 ->
      ERec w
      <$> (traverse . itraverse) (traverse . f) ds
      <*> itraverse (traverse . f) e0
    ECas w e0 cs -> ECas w <$> itraverse f e0 <*> (traverse . itraverse) f cs
    EMat w e0 as -> EMat w <$> itraverse f e0 <*> (traverse . itraverse) f as
    ETyAbs w x e -> ETyAbs w x <$> itraverse f e
    ETyApp w e t -> ETyApp w <$> itraverse f e <*> pure t

instance FunctorWithIndex     Pos (Case st tv) where
instance FoldableWithIndex    Pos (Case st tv) where
instance TraversableWithIndex Pos (Case st tv) where
  itraverse f (MkCase w c ts bs e0) = MkCase w c ts bs <$> itraverse (traverse . f) e0

instance FunctorWithIndex     Pos (Altn st tv) where
instance FoldableWithIndex    Pos (Altn st tv) where
instance TraversableWithIndex Pos (Altn st tv) where
  itraverse f (MkAltn w p e0) = MkAltn w p <$> itraverse (traverse . f) e0

instance Functor (StageType st) => Bifunctor (Defn st) where
  bimap f g (MkDefn b e) = MkDefn (fmap f b) (bimap f g e)

instance Functor (StageType st) => Bifunctor (Expr st) where
  bimap f g = \case
    EVar w x -> EVar w (g x)
    EVal w z -> EVal w z
    ECon w c -> ECon w c
    ENum w n -> ENum w n
    EApp w e0 es -> EApp w (bimap f g e0) (map (bimap f g) es)
    ELam w bs e0 -> ELam w (fmap (fmap f) bs) (bimap f (fmap g) e0)
    ELet w ds e0 -> ELet w (fmap (bimap f g) ds) (bimap f (fmap g) e0)
    ERec w ds e0 -> ERec w (fmap (bimap f (fmap g)) ds) (bimap f (fmap g) e0)
    ECas w e0 cs -> ECas w (bimap f g e0) (fmap (bimap f g) cs)
    EMat w e0 as -> EMat w (bimap f g e0) (fmap (bimap f g) as)
    ETyAbs w xs e0 -> ETyAbs w xs (bimap (fmap f) g e0)
    ETyApp w e0 ts -> ETyApp w (bimap f g e0) (fmap (fmap f) ts)

instance Functor (StageType st) => Bifunctor (Case st) where
  bimap f g (MkCase w c ts bs e) =
    MkCase w c (fmap (fmap f) ts) bs (bimap f (fmap g) e)

instance Functor (StageType st) => Bifunctor (Altn st) where
  bimap f g (MkAltn w p e) = MkAltn w (fmap f p) (bimap f (fmap g) e)

instance HasPos (TopLevel st) where
  pos f = \case
    TLTyp w tcs -> fmap (\w' -> TLTyp w' tcs) (f w)
    TLVal w x t -> fmap (\w' -> TLVal w' x t) (f w)
    TLDef      d -> fmap TLDef (pos f d)
    TLSup w z vs t bs e -> fmap (\w' -> TLSup w' z vs t bs e) (f w)
    TLAsm b    s -> fmap (\b' -> TLAsm b'    s) (pos f b)

instance HasPos (Defn st tv ev) where
  pos = defnLhs . bindPos

instance HasLhs (Defn st tv ev) where
  type Lhs (Defn st tv ev) = Id.EVar
  lhs = defnLhs . bindEVar

instance HasRhs (Defn st tv ev) where
  type Rhs (Defn st tv ev) = Expr st tv ev
  rhs = defnRhs

instance HasPos (Expr st tv ev) where
  pos f = \case
    EVar w x       -> fmap (\w' -> EVar w' x      ) (f w)
    EVal w z       -> fmap (\w' -> EVal w' z      ) (f w)
    ECon w c       -> fmap (\w' -> ECon w' c      ) (f w)
    ENum w n       -> fmap (\w' -> ENum w' n      ) (f w)
    EApp w t  us   -> fmap (\w' -> EApp w' t  us  ) (f w)
    ECas w t  cs   -> fmap (\w' -> ECas w' t  cs  ) (f w)
    ELam w ps t    -> fmap (\w' -> ELam w' ps t   ) (f w)
    ELet w ds t    -> fmap (\w' -> ELet w' ds t   ) (f w)
    ERec w ds t    -> fmap (\w' -> ERec w' ds t   ) (f w)
    EMat w ts as   -> fmap (\w' -> EMat w' ts as  ) (f w)
    ETyAbs w x e   -> fmap (\w' -> ETyAbs w' x e  ) (f w)
    ETyApp w e t   -> fmap (\w' -> ETyApp w' e t  ) (f w)

instance HasPos (Bind st tv) where
  pos = bindPos

instance HasLhs (Bind st tv) where
  type Lhs (Bind st tv) = Id.EVar
  lhs = bindEVar

instance HasPos (Altn st tv ev) where
  pos = altnPos

instance HasPos (Case st tv ev) where
  pos = casePos

-- * Pretty printing
class PrettyType f where
  pPrintPrecType :: (BaseTVar tv) => PrettyLevel -> Rational -> f tv -> Doc

instance PrettyType NoType where
  pPrintPrecType _ _ NoType = mempty

instance PrettyType Type where
  pPrintPrecType = pPrintPrec

type PrettyStage st = PrettyType (StageType st)

instance (HasTLTyp st ~ 'False, PrettyStage st) => Pretty (TopLevel st) where
  pPrintPrec lvl prec = \case
    TLVal _ x t ->
      "val" <+> pretty x <+> colon <+> pretty t
    TLDef     d -> "let" <+> pretty d
    TLSup _ z vs t bs e ->
      "let" <+>
      hang (pretty z <+> dvs_t <+> hsep (fmap pretty bs) <+> equals) 2
      (prettyETyAbs lvl prec vs e)
      where
        dt = pPrintPrecType lvl prec t
        dvs_t
          | isEmpty dt = empty
          | otherwise  = colon <+> prettyTUni lvl prec vs dt
    TLAsm   b s ->
      hsep ["external", pretty b, equals, text (show s)]

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
      maybeParens (prec > Op.aprec) $ hsep
      $ pPrintPrec lvl Op.aprec t : map (pPrintPrec lvl (Op.aprec+1)) us
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
    ELam _ bs e -> prettyELam lvl prec bs e
    -- If { _cond, _then, _else } ->
    --   maybeParens (prec > 0) $ sep
    --     [ "if"  <+> pPrintPrec lvl 0 _cond <+> "then"
    --     , nest 2 (pPrintPrec lvl 0 _then)
    --     , "else"
    --     , nest 2 (pPrintPrec lvl 0 _else)
    --     ]
    EMat _ t as ->
      maybeParens (prec > 0) $ vcat
      $ ("match" <+> pPrintPrec lvl 0 t <+> "with") : map (pPrintPrec lvl 0) (toList as)
    ECas _ t cs ->
      maybeParens (prec > 0) $ vcat
      $ ("match" <+> pPrintPrec lvl 0 t <+> "with") : map (pPrintPrec lvl 0) (toList cs)
    ETyAbs _ vs e -> prettyETyAbs lvl prec vs e
    ETyApp _ e0 ts ->
      maybeParens (prec > Op.aprec)
      $ pPrintPrec lvl Op.aprec e0 <+> prettyAtType (pPrintPrecType lvl 3) ts

prettyELam lvl prec bs e =
  maybeParens (prec > 0)
  $ hang ("fun" <+> hsep (fmap (pPrintPrec lvl 1) bs) <+> "->") 2 (pPrintPrec lvl 0 e)

prettyETyAbs lvl prec vs e
  | null vs   = pPrintPrec lvl prec e
  | otherwise =
      maybeParens (prec > 0)
      $ hang ("fun" <+> prettyAtType pretty vs <+> "->") 2 (pPrintPrec lvl 0 e)

prettyAtType :: Foldable t => (a -> Doc) -> t a -> Doc
prettyAtType p = hsep . map (\x -> "@" <> p x) . toList

instance (BaseTVar tv, PrettyStage st) => Pretty (Bind st tv) where
  pPrintPrec lvl prec (MkBind _ z t)
    | isEmpty td = zd
    | otherwise  = maybeParens (prec > 0) (zd <+> colon <+> td)
    where
      zd = pretty z
      td = pPrintPrecType lvl 0 t

instance (BaseEVar ev, BaseTVar tv, PrettyStage st) => Pretty (Case st tv ev) where
  pPrintPrec lvl _ (MkCase _ c ts bs e) =
    hang
      ( "|"
        <+> pretty c
        <+> prettyAtType (pPrintPrecType lvl 3) ts
        <+> hsep (fmap prettyBind bs) <+> "->"
      )
      2 (pPrintPrec lvl 0 e)
    where
      prettyBind = \case
        Nothing -> "_"
        Just x  -> pretty x

instance (BaseEVar ev, BaseTVar tv, PrettyStage st) => Pretty (Altn st tv ev) where
  pPrintPrec lvl _ (MkAltn _ p t) =
    hang ("|" <+> pPrintPrec lvl 0 p <+> "->") 2 (pPrintPrec lvl 0 t)

instance (BaseTVar tv, PrettyStage st) => Pretty (Patn st tv) where
  pPrintPrec lvl prec = \case
    PWld _      -> "_"
    PVar _ x    -> pretty x
    PCon _ c ts ps ->
      maybeParens (prec > 0 && (not (null ts) || not (null ps)))
      $ pretty c
        <+> prettyAtType (pPrintPrecType lvl 3) ts
        <+> hsep (map (pPrintPrec lvl 1) ps)

-- * Derived instances
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

deriving instance Functor     (StageType st) => Functor     (Patn st)
deriving instance Foldable    (StageType st) => Foldable    (Patn st)
deriving instance Traversable (StageType st) => Traversable (Patn st)

deriving instance Functor     (StageType st) => Functor     (Bind st)
deriving instance Foldable    (StageType st) => Foldable    (Bind st)
deriving instance Traversable (StageType st) => Traversable (Bind st)
