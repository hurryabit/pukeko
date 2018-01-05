{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Pukeko.Language.AST.Std
  ( GenModuleInfo (..)
  , ModuleInfo
  , Module (..)
  , TopLevel (..)
  , GenDefn (..)
  , Defn
  , Expr (..)
  , Case (..)
  , Altn (..)
  , Patn (..)
  , Bind (..)

  , abstract
  , (//)
  , bindName
  , patnToBind

  , module2tops
  , bind2evar
  , defn2dcon
  , patn2bind
  , case2rhs
  , altn2rhs

  , retagDefn
  , retagExpr

  , _BWild
  , _BName

  , prettyBinds

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
  = HasTLTyp st ~ 'True => TLTyp Pos [Con.TConDecl]
  | HasTLVal st ~ 'True => TLVal Pos Id.EVar (Type Id.TVar)
  | forall n.
    HasTLLet st ~ 'True => TLLet Pos (Vector n (Defn st Id.EVar))
  | forall n.
    HasTLLet st ~ 'True => TLRec Pos (Vector n (Defn st (EFinScope n Id.EVar)))
  | HasTLDef st ~ 'True => TLDef Pos Id.EVar (Expr st Id.EVar)
  | forall n.
    HasTLSup st ~ 'True => TLSup Pos Id.EVar (Vector n Bind) (Expr st (EFinScope n Id.EVar))
  | HasTLSup st ~ 'True => TLCaf Pos Id.EVar (Expr st Id.EVar)
  |                         TLAsm Pos Id.EVar String

data GenDefn expr v = MkDefn
  { _defnPos :: Pos
  , _defnLhs :: Id.EVar
  , _defnRhs :: expr v
  }
  deriving (Functor, Foldable, Traversable)

type Defn st = GenDefn (Expr st)

data Expr st v
  =           EVar Pos v
  |           ECon Pos Id.DCon
  |           ENum Pos Int
  |           EApp Pos (Expr st v) [Expr st v]
  | forall n. HasELam st ~ 'True => ELam Pos (Vector n Bind)   (Expr st (EFinScope n v))
  | forall n. ELet Pos (Vector n (Defn st v))              (Expr st (EFinScope n v))
  | forall n. ERec Pos (Vector n (Defn st (EFinScope n v))) (Expr st (EFinScope n v))
  | HasEMat st ~ 'False => ECas Pos (Expr st v) [Case st v]
  | HasEMat st ~ 'True => EMat Pos (Expr st v) [Altn st v]

data Case st v = forall n. MkCase
  { _casePos   :: Pos
  , _caseCon   :: Id.DCon
  , _caseBinds :: Vector n Bind
  , _caseRhs   :: Expr st (EFinScope n v)
  }

data Altn st v = MkAltn
  { _altnPos  :: Pos
  , _altnPatn :: Patn
  , _altnRhs  :: Expr st (EScope Id.EVar v)
  }

data Patn
  = PVar     Bind
  | PCon Pos Id.DCon [Patn]

data Bind
  = BWild Pos
  | BName Pos Id.EVar


-- * Derived optics
makeLenses ''GenDefn
makePrisms ''Bind


-- * Abstraction and substition

-- | Abstract all variables which are mapped to @Just@.
abstract :: (v -> Maybe (i, Id.EVar)) -> Expr st v -> Expr st (EScope i v)
abstract f = fmap (match f)
  where
    match :: (v -> Maybe (i, Id.EVar)) -> v -> EScope i v
    match f v = maybe (Free v) (uncurry mkBound) (f v)

-- | Replace subexpressions.
(//) :: Expr st v -> (Pos -> v -> Expr st w) -> Expr st w
expr // f = case expr of
  EVar w x       -> f w x
  ECon w c       -> ECon w c
  ENum w n       -> ENum w n
  EApp w t  us   -> EApp w (t // f) (map (// f) us)
  ECas w t  cs   -> ECas w (t // f) (map (over' case2rhs (/// f)) cs)
  ELam w ps t    -> ELam w ps (t /// f)
  ELet w ds t    -> ELet w (over (traverse . rhs1) (//  f) ds) (t /// f)
  ERec w ds t    -> ERec w (over (traverse . rhs1) (/// f) ds) (t /// f)
  EMat w t  as   -> EMat w (t // f) (map (over' altn2rhs (/// f)) as)

(///) :: Expr st (EScope i v) -> (Pos -> v -> Expr st w) -> Expr st (EScope i w)
t /// f = t // (\w x -> dist w (fmap (f w) x))

dist :: Pos -> EScope i (Expr st v) -> Expr st (EScope i v)
dist w (Bound i x) = EVar w (Bound i x)
dist _ (Free t)    = fmap Free t

-- * Getters
bindName :: Bind -> Maybe Id.EVar
bindName = \case
  BWild _   -> Nothing
  BName _ x -> Just x

patnToBind :: Patn -> Maybe Bind
patnToBind = \case
  PVar b -> Just b
  PCon{} -> Nothing

-- * Traversals
module2tops ::
  SameModuleInfo st1 st2 =>
  Lens (Module st1) (Module st2) [TopLevel st1] [TopLevel st2]
module2tops f (MkModule info tops) = MkModule info <$> f tops

-- TODO: Make this indexed if possible.
bind2evar :: Traversal' Bind Id.EVar
bind2evar f = \case
  BWild w   -> pure (BWild w)
  BName w x -> BName w <$> f x

-- * Deep traversals
type ExprConTraversal t =
  forall st1 st2 v. SameNodes st1 st2 =>
  IndexedTraversal Pos (t st1 v) (t st2 v) Id.DCon Id.DCon

defn2dcon :: ExprConTraversal Defn
defn2dcon = rhs2 . expr2dcon

expr2dcon :: ExprConTraversal Expr
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

case2dcon :: ExprConTraversal Case
case2dcon f (MkCase w c bs t) =
  MkCase w <$> indexed f w c <*> pure bs <*> expr2dcon f t

altn2dcon :: ExprConTraversal Altn
altn2dcon f (MkAltn w p t) =
  MkAltn w <$> patn2dcon f p <*> expr2dcon f t

patn2dcon :: IndexedTraversal' Pos Patn Id.DCon
patn2dcon f = \case
  PVar   b    -> pure $ PVar b
  PCon w c ps -> PCon w <$> indexed f w c <*> (traverse . patn2dcon) f ps

patn2bind :: IndexedTraversal' Pos Patn Bind
patn2bind f = \case
  PVar   b    -> PVar <$> indexed f (b^.pos) b
  PCon w c ps -> PCon w c <$> (traverse . patn2bind) f ps

-- * Highly polymorphic lenses
over' ::
  ((forall i. g (s i v1) -> Identity (g (s i v2))) -> f v1 -> Identity (f v2)) ->
   (forall i. g (s i v1) ->           g (s i v2))  -> f v1 ->           f v2
over' l f = runIdentity . l (Identity . f)

case2rhs
  :: (Functor f)
  => (forall i. IsVarLevel i => Expr st1 (EScope i v1) -> f (Expr st2 (EScope i v2)))
  -> Case st1 v1 -> f (Case st2 v2)
case2rhs f (MkCase w c bs t) = MkCase w c bs <$> f t

altn2rhs
  :: (Functor f)
  => (forall i. IsVarLevel i => Expr st1 (EScope i v1) -> f (Expr st2 (EScope i v2)))
  -> Altn st1 v1 -> f (Altn st2 v2)
altn2rhs f (MkAltn w p t) = MkAltn w p <$> f t

-- * Retagging
retagDefn ::
  (SameNodes st1 st2) =>
  Defn st1 v -> Defn st2 v
retagDefn = over defn2dcon id

retagExpr ::
  forall st1 st2 v. (SameNodes st1 st2) =>
  Expr st1 v -> Expr st2 v
retagExpr = over expr2dcon id

-- * Manual instances
instance TraversableWithIndex Pos expr => FunctorWithIndex     Pos (GenDefn expr) where
instance TraversableWithIndex Pos expr => FoldableWithIndex    Pos (GenDefn expr) where
instance TraversableWithIndex Pos expr => TraversableWithIndex Pos (GenDefn expr) where
  itraverse f (MkDefn w x e) = MkDefn w x <$> itraverse f e

instance FunctorWithIndex     Pos (Expr st) where
instance FoldableWithIndex    Pos (Expr st) where
instance TraversableWithIndex Pos (Expr st) where
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

instance FunctorWithIndex     Pos (Case st) where
instance FoldableWithIndex    Pos (Case st) where
instance TraversableWithIndex Pos (Case st) where
  itraverse f (MkCase w c bs e0) = MkCase w c bs <$> itraverse (traverse . f) e0

instance FunctorWithIndex     Pos (Altn st) where
instance FoldableWithIndex    Pos (Altn st) where
instance TraversableWithIndex Pos (Altn st) where
  itraverse f (MkAltn w p e0) = MkAltn w p <$> itraverse (traverse . f) e0



instance HasPos (GenDefn expr v) where
  pos = defnPos

instance HasLhs (GenDefn expr v) where
  type Lhs (GenDefn expr v) = Id.EVar
  lhs = defnLhs

instance HasRhs (GenDefn expr v) where
  type Rhs (GenDefn expr v) = expr v
  rhs = defnRhs

instance HasRhs1 (GenDefn expr) where
  type Rhs1 (GenDefn expr) = expr
  rhs1 = defnRhs

instance HasRhs2 GenDefn where
  rhs2 = defnRhs

instance HasPos (Expr std v) where
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

instance HasPos Bind where
  pos f = \case
    BWild w   -> fmap         BWild       (f w)
    BName w x -> fmap (\w' -> BName w' x) (f w)

-- * Pretty printing
instance (HasTLTyp st ~ 'False) => Pretty (TopLevel st) where
  pPrintPrec _ _ = \case
    TLVal _ x t ->
      "val" <+> pretty x <+> colon <+> pretty t
    TLLet _ ds -> prettyDefns False ds
    TLRec _ ds -> prettyDefns True  ds
    TLDef w x e -> "let" <+> pretty (MkDefn w x e)
    TLSup _ x bs e ->
      "let" <+> hang (pretty x <+> prettyBinds bs <+> equals) 2 (pretty e)
    TLCaf _ x t ->
      "let" <+> hang (pretty x <+> equals) 2 (pretty t)
    TLAsm _ x s ->
      hsep ["external", pretty x, equals, text (show s)]

instance (IsEVar v) => Pretty (Defn st v) where
  pPrintPrec lvl _ (MkDefn _ x t) =
    hang (pPrintPrec lvl 0 x <+> equals) 2 (pPrintPrec lvl 0 t)

prettyDefns :: (IsEVar v) => Bool -> Vector n (Defn st v) -> Doc
prettyDefns isrec ds = case toList ds of
    [] -> mempty
    d0:ds -> vcat ((let_ <+> pretty d0) : map (\d -> "and" <+> pretty d) ds)
    where
      let_ | isrec     = "let rec"
           | otherwise = "let"

instance (IsEVar v) => Pretty (Expr st v) where
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
        [ "fun", prettyBinds bs
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

instance (IsEVar v) => Pretty (Case st v) where
  pPrintPrec lvl _ (MkCase _ c bs t) =
    hang ("|" <+> pretty c <+> prettyBinds bs <+> "->") 2 (pPrintPrec lvl 0 t)

instance (IsEVar v) => Pretty (Altn st v) where
  pPrintPrec lvl _ (MkAltn _ p t) =
    hang ("|" <+> pPrintPrec lvl 0 p <+> "->") 2 (pPrintPrec lvl 0 t)

instance Pretty Patn where
  pPrintPrec lvl prec = \case
    PVar   b    -> pretty b
    PCon _ c ps ->
      maybeParens (prec > 0 && not (null ps)) $
      pretty c <+> hsep (map (pPrintPrec lvl 1) (toList ps))

instance Pretty Bind where
  pPrint = \case
    BWild _   -> "_"
    BName _ x -> pretty x

prettyBinds :: Vector n Bind -> Doc
prettyBinds = hsep . map pretty . toList

-- * Derived instances
deriving instance Functor     (Expr st)
deriving instance Foldable    (Expr st)
deriving instance Traversable (Expr st)

deriving instance Functor     (Case st)
deriving instance Foldable    (Case st)
deriving instance Traversable (Case st)

deriving instance Functor     (Altn st)
deriving instance Foldable    (Altn st)
deriving instance Traversable (Altn st)
