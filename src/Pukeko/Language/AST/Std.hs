{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
module Pukeko.Language.AST.Std
  ( Stage (..)
  , GenDefn (..)
  , StdDefn
  , StdExpr (..)
  , StdCase (..)
  , StdAltn (..)
  , GenPatn (..)
  , StdPatn
  , Bind (..)

  , abstract
  , (//)
  , bindName
  , patnToBind

  , defn2exprCon
  , patn2bind
  , case2rhs
  , altn2rhs

  , retagDefn
  , retagExpr

  , _Wild
  , _Name

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
import           Pukeko.Language.AST.Classes
import           Pukeko.Language.AST.Scope

class Stage st where
  type ExprConOf st :: *
  type HasLam    st :: Bool
  type HasMat    st :: Bool

type PrettyStage st = (Pretty (ExprConOf st))

type SameNodes st1 st2 = (HasLam st1 ~ HasLam st2, HasMat st1 ~ HasMat st2)

data GenDefn expr v = MkDefn
  { _defnPos :: Pos
  , _defnLhs :: Id.EVar
  , _defnRhs :: expr v
  }
  deriving (Functor, Foldable, Traversable)

type StdDefn st = GenDefn (StdExpr st)

data StdExpr st v
  =           Var Pos v
  |           Con Pos (ExprConOf st)
  |           Num Pos Int
  |           App Pos (StdExpr st v) [StdExpr st v]
  | forall n. HasLam st ~ 'True => Lam Pos (Vector n Bind)   (StdExpr st (FinScope n v))
  | forall n. Let Pos (Vector n (StdDefn st v))              (StdExpr st (FinScope n v))
  | forall n. Rec Pos (Vector n (StdDefn st (FinScope n v))) (StdExpr st (FinScope n v))
  | HasMat st ~ 'False => Cas Pos (StdExpr st v) [StdCase st v]
  | HasMat st ~ 'True => Mat Pos (StdExpr st v) [StdAltn st v]

data StdCase st v = forall n. MkCase
  { _casePos   :: Pos
  , _caseCon   :: ExprConOf st
  , _caseBinds :: Vector n Bind
  , _caseRhs   :: StdExpr st (FinScope n v)
  }

data StdAltn st v = MkAltn
  { _altnPos  :: Pos
  , _altnPatn :: StdPatn st
  , _altnRhs  :: StdExpr st (Scope Id.EVar v)
  }

data GenPatn con
  = Bind     Bind
  | Dest Pos con [GenPatn con]

type StdPatn st = GenPatn (ExprConOf st)

data Bind
  = Wild Pos
  | Name Pos Id.EVar


-- * Derived optics
makeLenses ''GenDefn
makePrisms ''Bind


-- * Abstraction and substition

-- | Abstract all variables which are mapped to @Just@.
abstract :: (v -> Maybe (i, Id.EVar)) -> StdExpr st v -> StdExpr st (Scope i v)
abstract f = fmap (match f)
  where
    match :: (v -> Maybe (i, Id.EVar)) -> v -> Scope i v
    match f v = maybe (Free v) (uncurry bound) (f v)

-- | Replace subexpressions.
(//) :: StdExpr st v -> (Pos -> v -> StdExpr st w) -> StdExpr st w
expr // f = case expr of
  Var w x       -> f w x
  Con w c       -> Con w c
  Num w n       -> Num w n
  App w t  us   -> App w (t // f) (map (// f) us)
  -- If  w t  u  v -> If  w (t // f) (u // f) (v // f)
  Cas w t  cs   -> Cas w (t // f) (map (over' case2rhs (/// f)) cs)
  Lam w ps t    -> Lam w ps (t /// f)
  Let w ds t    -> Let w (over (traverse . rhs1) (//  f) ds) (t /// f)
  Rec w ds t    -> Rec w (over (traverse . rhs1) (/// f) ds) (t /// f)
  Mat w t  as   -> Mat w (t // f) (map (over' altn2rhs (/// f)) as)

(///) :: StdExpr st (Scope i v) -> (Pos -> v -> StdExpr st w) -> StdExpr st (Scope i w)
t /// f = t // (\w x -> dist w (fmap (f w) x))

dist :: Pos -> Scope i (StdExpr st v) -> StdExpr st (Scope i v)
dist w (Bound i x) = Var w (Bound i x)
dist _ (Free t)    = fmap Free t

-- * Getters
bindName :: Bind -> Maybe Id.EVar
bindName = \case
  Wild _   -> Nothing
  Name _ x -> Just x

patnToBind :: GenPatn con -> Maybe Bind
patnToBind = \case
  Bind b -> Just b
  Dest{} -> Nothing

-- * Deep traversals
type ExprConTraversal t =
  forall st1 st2 v. SameNodes st1 st2 =>
  IndexedTraversal Pos (t st1 v) (t st2 v) (ExprConOf st1) (ExprConOf st2)

defn2exprCon :: ExprConTraversal StdDefn
defn2exprCon = rhs2 . expr2exprCon

expr2exprCon :: ExprConTraversal StdExpr
expr2exprCon f = \case
  Var w x       -> pure $ Var w x
  Con w c       -> Con w <$> indexed f w c
  Num w n       -> pure $ Num w n
  App w t  us   -> App w <$> expr2exprCon f t <*> (traverse . expr2exprCon) f us
  Cas w t  cs   -> Cas w <$> expr2exprCon f t <*> (traverse . case2exprCon) f cs
  Lam w bs t    -> Lam w bs <$> expr2exprCon f t
  Let w ds t    -> Let w <$> (traverse . defn2exprCon) f ds <*> expr2exprCon f t
  Rec w ds t    -> Rec w <$> (traverse . defn2exprCon) f ds <*> expr2exprCon f t
  Mat w t  as   -> Mat w <$> expr2exprCon f t <*> (traverse . altn2exprCon) f as

case2exprCon :: ExprConTraversal StdCase
case2exprCon f (MkCase w c bs t) =
  MkCase w <$> indexed f w c <*> pure bs <*> expr2exprCon f t

altn2exprCon :: ExprConTraversal StdAltn
altn2exprCon f (MkAltn w p t) =
  MkAltn w <$> patn2exprCon f p <*> expr2exprCon f t

patn2exprCon ::
  IndexedTraversal Pos (GenPatn con1) (GenPatn con2) con1 con2
patn2exprCon f = \case
  Bind   b    -> pure $ Bind b
  Dest w c ps -> Dest w <$> indexed f w c <*> (traverse . patn2exprCon) f ps

patn2bind :: IndexedTraversal' Pos (GenPatn con) Bind
patn2bind f = \case
  Bind   b    -> Bind <$> indexed f (b^.pos) b
  Dest w c ps -> Dest w c <$> (traverse . patn2bind) f ps

-- * Highly polymorphic lenses
over' ::
  ((forall i. g (s i v1) -> Identity (g (s i v2))) -> f v1 -> Identity (f v2)) ->
   (forall i. g (s i v1) ->           g (s i v2))  -> f v1 ->           f v2
over' l f = runIdentity . l (Identity . f)

case2rhs
  :: (Functor f, ExprConOf st1 ~ ExprConOf st2)
  => (forall i. Ord i => StdExpr st1 (Scope i v1) -> f (StdExpr st2 (Scope i v2)))
  -> StdCase st1 v1 -> f (StdCase st2 v2)
case2rhs f (MkCase w c bs t) = MkCase w c bs <$> f t

altn2rhs
  :: (Functor f, ExprConOf st1 ~ ExprConOf st2)
  => (forall i. Ord i => StdExpr st1 (Scope i v1) -> f (StdExpr st2 (Scope i v2)))
  -> StdAltn st1 v1 -> f (StdAltn st2 v2)
altn2rhs f (MkAltn w p t) = MkAltn w p <$> f t

-- * Retagging
retagDefn ::
  (ExprConOf st1 ~ ExprConOf st2, SameNodes st1 st2) =>
  StdDefn st1 v -> StdDefn st2 v
retagDefn = over defn2exprCon id

retagExpr ::
  (ExprConOf st1 ~ ExprConOf st2, SameNodes st1 st2) =>
  StdExpr st1 v -> StdExpr st2 v
retagExpr = over expr2exprCon id

-- * Manual instances
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

instance HasPos (StdExpr std v) where
  pos f = \case
    Var w x       -> fmap (\w' -> Var w' x      ) (f w)
    Con w c       -> fmap (\w' -> Con w' c      ) (f w)
    Num w n       -> fmap (\w' -> Num w' n      ) (f w)
    App w t  us   -> fmap (\w' -> App w' t  us  ) (f w)
    Cas w t  cs   -> fmap (\w' -> Cas w' t  cs  ) (f w)
    Lam w ps t    -> fmap (\w' -> Lam w' ps t   ) (f w)
    Let w ds t    -> fmap (\w' -> Let w' ds t   ) (f w)
    Rec w ds t    -> fmap (\w' -> Rec w' ds t   ) (f w)
    Mat w ts as   -> fmap (\w' -> Mat w' ts as  ) (f w)

instance HasPos Bind where
  pos f = \case
    Wild w   -> fmap         Wild       (f w)
    Name w x -> fmap (\w' -> Name w' x) (f w)

-- * Pretty printing
instance (PrettyStage st, IsVar v) => Pretty (StdDefn st v) where
  pPrintPrec lvl _ (MkDefn _ x t) =
    hang (pPrintPrec lvl 0 x <+> equals) 2 (pPrintPrec lvl 0 t)

instance (PrettyStage st, IsVar v) => Pretty (StdExpr st v) where
  pPrintPrec lvl prec = \case
    Var _ x -> pretty (varName x)
    Con _ c -> pretty c
    Num _ n -> int n
    App _ t us ->
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
    Let _ ds t ->
      case toList ds of
        [] -> pPrintPrec lvl 0 t
        d0:ds -> vcat
          [ sep
            [ vcat $
              ("let" <+> pPrintPrec lvl 0 d0) :
                map (\d -> "and" <+> pPrintPrec lvl 0 d) ds
            , "in"
            ]
          , pPrintPrec lvl 0 t
          ]
    Rec _ ds t ->
      case toList ds of
        [] -> pPrintPrec lvl 0 t
        d0:ds -> vcat
          [ sep
            [ vcat $
              ("let rec" <+> pPrintPrec lvl 0 d0) :
                map (\d -> "and" <+> pPrintPrec lvl 0 d) ds
            , "in"
            ]
          , pPrintPrec lvl 0 t
          ]
    Lam _ bs t ->
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
    Mat _ t as ->
      maybeParens (prec > 0) $ vcat
      $ ("match" <+> pPrintPrec lvl 0 t <+> "with") : map (pPrintPrec lvl 0) as
    Cas _ t cs ->
      maybeParens (prec > 0) $ vcat
      $ ("match" <+> pPrintPrec lvl 0 t <+> "with") : map (pPrintPrec lvl 0) cs

instance (PrettyStage st, IsVar v) => Pretty (StdCase st v) where
  pPrintPrec lvl _ (MkCase _ c bs t) =
    hang ("|" <+> pretty c <+> prettyBinds bs <+> "->") 2 (pPrintPrec lvl 0 t)

instance (PrettyStage st, IsVar v) => Pretty (StdAltn st v) where
  pPrintPrec lvl _ (MkAltn _ p t) =
    hang ("|" <+> pPrintPrec lvl 0 p <+> "->") 2 (pPrintPrec lvl 0 t)

instance Pretty con => Pretty (GenPatn con) where
  pPrintPrec lvl prec = \case
    Bind   b    -> pretty b
    Dest _ c ps ->
      maybeParens (prec > 0 && not (null ps)) $
      pretty c <+> hsep (map (pPrintPrec lvl 1) (toList ps))

instance Pretty Bind where
  pPrint = \case
    Wild _   -> "_"
    Name _ x -> pretty x

prettyBinds :: Vector n Bind -> Doc
prettyBinds = hsep . map pretty . toList

-- * Derived instances
deriving instance Functor     (StdExpr st)
deriving instance Foldable    (StdExpr st)
deriving instance Traversable (StdExpr st)

deriving instance Functor     (StdCase st)
deriving instance Foldable    (StdCase st)
deriving instance Traversable (StdCase st)

deriving instance Functor     (StdAltn st)
deriving instance Foldable    (StdAltn st)
deriving instance Traversable (StdAltn st)