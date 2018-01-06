{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Pukeko.Language.AST.Std
  ( GenModuleInfo (..)
  , ModuleInfo
  , Module (..)
  , TopLevel (..)
  , Defn (..)
  , Expr (..)
  , Case (..)
  , Altn (..)
  , Patn (..)

  , abstract
  , (//)

  , module2tops
  , defn2rhs
  , defn2dcon
  , patn2evar
  , case2rhs
  , altn2rhs

  , retagDefn
  , retagExpr

  , Pos
  , module Pukeko.Language.AST.Scope
  )
  where

import Control.Lens
import Data.Vector.Sized (Vector)
import Data.Foldable

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
    TLVal Pos Id.EVar TypeSchema
  | forall n. HasTLLet st ~ 'True =>
    TLLet Pos (Vector n (Defn st Void Id.EVar))
  | forall n. HasTLLet st ~ 'True =>
    TLRec Pos (Vector n (Defn st Void (EFinScope n Id.EVar)))
  | HasTLDef st ~ 'True =>
    TLDef Pos Id.EVar (Expr st Void Id.EVar)
  | forall n. HasTLSup st ~ 'True =>
    TLSup Pos Id.EVar (Vector n Id.EVar) (Expr st Void (EFinScope n Id.EVar))
  | HasTLSup st ~ 'True =>
    TLCaf Pos Id.EVar (Expr st Void Id.EVar)
  | TLAsm Pos Id.EVar String

data Defn st tv ev = MkDefn
  { _defnPos :: Pos
  , _defnLhs :: Id.EVar
  , _defnRhs :: Expr st tv ev
  }

data Expr st tv ev
  = EVar Pos ev
  | ECon Pos Id.DCon
  | ENum Pos Int
  | EApp Pos (Expr st tv ev) [Expr st tv ev]
  | forall n. HasELam st ~ 'True =>
    ELam Pos (Vector n Id.EVar)                       (Expr st tv (EFinScope n ev))
  | forall n.
    ELet Pos (Vector n (Defn st tv ev))               (Expr st tv (EFinScope n ev))
  | forall n.
    ERec Pos (Vector n (Defn st tv (EFinScope n ev))) (Expr st tv (EFinScope n ev))
  | HasEMat st ~ 'False =>
    ECas Pos (Expr st tv ev) [Case st tv ev]
  | HasEMat st ~ 'True =>
    EMat Pos (Expr st tv ev) [Altn st tv ev]
  | HasETyp st ~ 'True =>
    ETyAbs Pos Id.TVar (Expr st (TScope1 tv) ev)
  | HasETyp st ~ 'True =>
    ETyApp Pos (Expr st tv ev) (Type tv)

data Case st tv ev = forall n. MkCase
  { _casePos   :: Pos
  , _caseCon   :: Id.DCon
  , _caseBinds :: Vector n (Maybe Id.EVar)
  , _caseRhs   :: Expr st tv (EFinScope n ev)
  }

data Altn st tv ev = MkAltn
  { _altnPos  :: Pos
  , _altnPatn :: Patn
  , _altnRhs  :: Expr st tv (EScope Id.EVar ev)
  }

data Patn
  = PWld Pos
  | PVar Pos Id.EVar
  | PCon Pos Id.DCon [Patn]

-- * Derived optics
makeLenses ''Defn

-- * Abstraction and substition

-- | Abstract all variables which are mapped to @Just@.
abstract :: (ev -> Maybe (i, Id.EVar)) -> Expr st tv ev -> Expr st tv (EScope i ev)
abstract f = fmap (match f)
  where
    match :: (v -> Maybe (i, Id.EVar)) -> v -> EScope i v
    match f v = maybe (Free v) (uncurry mkBound) (f v)

-- | Replace subexpressions.
(//) :: Expr st tv ev1 -> (forall tv. Pos -> ev1 -> Expr st tv ev2) -> Expr st tv ev2
expr // f = case expr of
  EVar w x       -> f w x
  ECon w c       -> ECon w c
  ENum w n       -> ENum w n
  EApp w t  us   -> EApp w (t // f) (map (// f) us)
  ECas w t  cs   -> ECas w (t // f) (map (over' case2rhs (/// f)) cs)
  ELam w ps t    -> ELam w ps (t /// f)
  ELet w ds t    -> ELet w (over (traverse . defn2rhs) (//  f) ds) (t /// f)
  ERec w ds t    -> ERec w (over (traverse . defn2rhs) (/// f) ds) (t /// f)
  EMat w t  as   -> EMat w (t // f) (map (over' altn2rhs (/// f)) as)
  ETyAbs w x e   -> ETyAbs w x (e // f)
  ETyApp w e t   -> ETyApp w (e // f) t

(///) ::
  Expr st tv (EScope i ev1)                 ->
  (forall tv. Pos -> ev1 -> Expr st tv ev2) ->
  Expr st tv (EScope i ev2)
t /// f = t // (\w x -> dist w (fmap (f w) x))

dist :: Pos -> EScope i (Expr st tv ev) -> Expr st tv (EScope i ev)
dist w (Bound i x) = EVar w (Bound i x)
dist _ (Free t)    = fmap Free t

-- * Lenses and traversals
module2tops ::
  SameModuleInfo st1 st2 =>
  Lens (Module st1) (Module st2) [TopLevel st1] [TopLevel st2]
module2tops f (MkModule info tops) = MkModule info <$> f tops

defn2rhs :: Lens (Defn st1 tv ev1) (Defn st2 tv ev2) (Expr st1 tv ev1) (Expr st2 tv ev2)
defn2rhs f (MkDefn w x e) = MkDefn w x <$> f e

-- * Deep traversals
type DConTraversal t =
  forall st1 st2 tv ev. SameNodes st1 st2 =>
  IndexedTraversal Pos (t st1 tv ev) (t st2 tv ev) Id.DCon Id.DCon

defn2dcon :: DConTraversal Defn
defn2dcon f (MkDefn w x e) = MkDefn w x <$> expr2dcon f e

expr2dcon :: DConTraversal Expr
expr2dcon f = \case
  EVar w x       -> pure (EVar w x)
  ECon w c       -> ECon w <$> indexed f w c
  ENum w n       -> pure (ENum w n)
  EApp w t  us   -> EApp w <$> expr2dcon f t <*> (traverse . expr2dcon) f us
  ECas w t  cs   -> ECas w <$> expr2dcon f t <*> (traverse . case2dcon) f cs
  ELam w bs t    -> ELam w bs <$> expr2dcon f t
  ELet w ds t    -> ELet w <$> (traverse . defn2dcon) f ds <*> expr2dcon f t
  ERec w ds t    -> ERec w <$> (traverse . defn2dcon) f ds <*> expr2dcon f t
  EMat w t  as   -> EMat w <$> expr2dcon f t <*> (traverse . altn2dcon) f as
  ETyAbs w x e   -> ETyAbs w x <$> expr2dcon f e
  ETyApp w e t   -> ETyApp w <$> expr2dcon f e <*> pure t

case2dcon :: DConTraversal Case
case2dcon f (MkCase w c bs t) =
  MkCase w <$> indexed f w c <*> pure bs <*> expr2dcon f t

altn2dcon :: DConTraversal Altn
altn2dcon f (MkAltn w p t) =
  MkAltn w <$> patn2dcon f p <*> expr2dcon f t

patn2dcon :: IndexedTraversal' Pos Patn Id.DCon
patn2dcon f = \case
  PWld w      -> pure (PWld w)
  PVar w x    -> pure (PVar w x)
  PCon w c ps -> PCon w <$> indexed f w c <*> (traverse . patn2dcon) f ps

patn2evar :: IndexedTraversal' Pos Patn Id.EVar
patn2evar f = \case
  PWld w      -> pure (PWld w)
  PVar w x    -> PVar w <$> indexed f w x
  PCon w c ps -> PCon w c <$> (traverse . patn2evar) f ps

-- * Scoped lenses
type ScopedLens scope s t a b =
  forall f v w. (Functor f) =>
  (forall i. a (scope i v) -> f (b (scope i w))) -> s v -> f (t w)

over' ::
  ((forall i. g (s i v1) -> Identity (g (s i v2))) -> f v1 -> Identity (f v2)) ->
   (forall i. g (s i v1) ->           g (s i v2))  -> f v1 ->           f v2
over' l f = runIdentity . l (Identity . f)

case2rhs :: ScopedLens EScope (Case st1 tv) (Case st2 tv) (Expr st1 tv) (Expr st2 tv)
case2rhs f (MkCase w c bs t) = MkCase w c bs <$> f t

altn2rhs :: ScopedLens EScope (Altn st1 tv) (Altn st2 tv) (Expr st1 tv) (Expr st2 tv)
altn2rhs f (MkAltn w p t) = MkAltn w p <$> f t

-- * Retagging
retagDefn :: (SameNodes st1 st2) => Defn st1 tv ev -> Defn st2 tv ev
retagDefn = over defn2dcon id

retagExpr :: (SameNodes st1 st2) => Expr st1 tv ev -> Expr st2 tv ev
retagExpr = over expr2dcon id

-- * Manual instances
instance FunctorWithIndex     Pos (Defn st tv) where
instance FoldableWithIndex    Pos (Defn st tv) where
instance TraversableWithIndex Pos (Defn st tv) where
  itraverse f (MkDefn w x e) = MkDefn w x <$> itraverse f e

instance FunctorWithIndex     Pos (Expr st tv) where
instance FoldableWithIndex    Pos (Expr st tv) where
instance TraversableWithIndex Pos (Expr st tv) where
  itraverse f = \case
    EVar w x -> EVar w <$> f w x
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
  itraverse f (MkCase w c bs e0) = MkCase w c bs <$> itraverse (traverse . f) e0

instance FunctorWithIndex     Pos (Altn st tv) where
instance FoldableWithIndex    Pos (Altn st tv) where
instance TraversableWithIndex Pos (Altn st tv) where
  itraverse f (MkAltn w p e0) = MkAltn w p <$> itraverse (traverse . f) e0



instance HasPos (Defn st tv ev) where
  pos = defnPos

instance HasLhs (Defn st tv ev) where
  type Lhs (Defn st tv ev) = Id.EVar
  lhs = defnLhs

instance HasRhs (Defn st tv ev) where
  type Rhs (Defn st tv ev) = Expr st tv ev
  rhs = defnRhs

instance HasPos (Expr std tv ev) where
  pos f = \case
    EVar w x       -> fmap (\w' -> EVar w' x      ) (f w)
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

-- * Pretty printing
instance (HasTLTyp st ~ 'False) => Pretty (TopLevel st) where
  pPrintPrec _ _ = \case
    TLVal _ x t ->
      "val" <+> pretty x <+> colon <+> pretty t
    TLLet _ ds -> prettyDefns False ds
    TLRec _ ds -> prettyDefns True  ds
    TLDef w x e -> "let" <+> pretty (MkDefn w x e)
    TLSup _ x bs e ->
      "let" <+> hang (pretty x <+> hsep (fmap pretty bs) <+> equals) 2 (pretty e)
    TLCaf _ x t ->
      "let" <+> hang (pretty x <+> equals) 2 (pretty t)
    TLAsm _ x s ->
      hsep ["external", pretty x, equals, text (show s)]

instance (IsEVar ev, IsTVar tv) => Pretty (Defn st tv ev) where
  pPrintPrec lvl _ (MkDefn _ x t) =
    hang (pPrintPrec lvl 0 x <+> equals) 2 (pPrintPrec lvl 0 t)

prettyDefns :: (IsEVar ev, IsTVar tv) => Bool -> Vector n (Defn st tv ev) -> Doc
prettyDefns isrec ds = case toList ds of
    [] -> mempty
    d0:ds -> vcat ((let_ <+> pretty d0) : map (\d -> "and" <+> pretty d) ds)
    where
      let_ | isrec     = "let rec"
           | otherwise = "let"

instance (IsEVar ev, IsTVar tv) => Pretty (Expr st tv ev) where
  pPrintPrec lvl prec = \case
    EVar _ x -> pretty (baseName x)
    ECon _ c -> pretty c
    ENum _ n -> int n
    EApp _ t us ->
      maybeParens (prec > Op.aprec) $ hsep
      $ pPrintPrec lvl Op.aprec t : map (pPrintPrec lvl (Op.aprec+1)) us
    -- TODO: Bring this back in Ap when _fun is an operator.
    -- ApOp   { _op, _arg1, _arg2 } ->
    --   let MkSpec { _sym, _prec, _assoc } = Operator.findByName _op
    --       (prec1, prec2) =
    --         case _assoc of
    --           AssocLeft  -> (_prec  , _prec+1)
    --           AssocRight -> (_prec+1, _prec  )
    --           AssocNone  -> (_prec+1, _prec+1)
    --   in  maybeParens (prec > _prec) $
    --         pPrintPrec lvl prec1 _arg1 <> text _sym <> pPrintPrec lvl prec2 _arg2
    -- TODO: Avoid this code duplication.
    ELet _ ds t -> sep [prettyDefns False ds, "in"] $$ pPrintPrec lvl 0 t
    ERec _ ds t -> sep [prettyDefns True  ds, "in"] $$ pPrintPrec lvl 0 t
    ELam _ bs t ->
      maybeParens (prec > 0) $ hsep
        [ "fun", hsep (fmap pretty bs)
        , "->" , pPrintPrec lvl 0 t
        ]
    -- If { _cond, _then, _else } ->
    --   maybeParens (prec > 0) $ sep
    --     [ "if"  <+> pPrintPrec lvl 0 _cond <+> "then"
    --     , nest 2 (pPrintPrec lvl 0 _then)
    --     , "else"
    --     , nest 2 (pPrintPrec lvl 0 _else)
    --     ]
    EMat _ t as ->
      maybeParens (prec > 0) $ vcat
      $ ("match" <+> pPrintPrec lvl 0 t <+> "with") : map (pPrintPrec lvl 0) as
    ECas _ t cs ->
      maybeParens (prec > 0) $ vcat
      $ ("match" <+> pPrintPrec lvl 0 t <+> "with") : map (pPrintPrec lvl 0) cs
    ETyAbs _ x e ->
      maybeParens (prec > 0)
      $ "fun" <+> "@" <> pretty x <+> "->" <+> pPrintPrec lvl prec e
    ETyApp _ e t ->
      maybeParens (prec > Op.aprec)
      $ pPrintPrec lvl Op.aprec e <+> "@" <> pPrintPrec lvl 3 t

instance (IsEVar ev, IsTVar tv) => Pretty (Case st tv ev) where
  pPrintPrec lvl _ (MkCase _ c bs t) =
    hang ("|" <+> pretty c <+> hsep (fmap prettyBind bs) <+> "->")
      2 (pPrintPrec lvl 0 t)
    where
      prettyBind = \case
        Nothing -> "_"
        Just x  -> pretty x


instance (IsEVar ev, IsTVar tv) => Pretty (Altn st tv ev) where
  pPrintPrec lvl _ (MkAltn _ p t) =
    hang ("|" <+> pPrintPrec lvl 0 p <+> "->") 2 (pPrintPrec lvl 0 t)

instance Pretty Patn where
  pPrintPrec lvl prec = \case
    PWld _      -> "_"
    PVar _ x    -> pretty x
    PCon _ c ps ->
      maybeParens (prec > 0 && not (null ps)) $
      pretty c <+> hsep (map (pPrintPrec lvl 1) (toList ps))

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
