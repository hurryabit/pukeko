module Pukeko.Language.LambdaLifter.AST
  ( TypeCon
  , ExprCon
  , Module
  , TopLevel (..)
  , Defn
  , Expr (..)
  , Altn
  , Patn
  )
where

import           Data.Foldable                   (toList)

import           Pukeko.Pretty
import           Pukeko.Language.Base.AST
import           Pukeko.Language.DeadCode.AST    (TypeCon, ExprCon)
import qualified Pukeko.Language.Ident           as Id
import           Pukeko.Language.Operator        (aprec)

type Module = [TopLevel]

data TopLevel
  = forall n. Def Pos Id.EVar (Vector n Bind) (Expr (FinScope n Id.EVar))
  |           Caf Pos Id.EVar (Expr Id.EVar)
  |           Asm Pos Id.EVar String

type Defn = StdDefn Expr

data Expr v
  =           Var Pos v
  |           Con Pos ExprCon
  |           Num Pos Int
  |           App Pos (Expr v) [Expr v]
  -- |           If  Pos (Expr v) (Expr v) (Expr v)
  |           Mat Pos (Expr v) [Altn v]
  | forall n. Let Pos (Vector n (Defn v))              (Expr (FinScope n v))
  | forall n. Rec Pos (Vector n (Defn (FinScope n v))) (Expr (FinScope n v))

type Altn = StdAltn ExprCon Expr

type Patn = StdPatn ExprCon

deriving instance Functor     Expr
deriving instance Foldable    Expr
deriving instance Traversable Expr

instance IsVar v => Pretty (Expr v) where
  pPrintPrec lvl prec = \case
    Var _ x -> pretty (varName x)
    Con _ c -> pretty c
    Num _ n -> int n
    App _ t us ->
      maybeParens (prec > aprec) $ hsep
      $ pPrintPrec lvl aprec t : map (pPrintPrec lvl (aprec+1)) us
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
    -- Lam{_patns, _body} ->
    --   maybeParens (prec > 0) $ hsep
    --     [ "fun", hsep (map (pPrintPrec lvl 1) _patns)
    --     , "->" , pPrintPrec lvl 0 _body
    --     ]
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

instance IsVar v => Pretty (Defn v) where
  pPrintPrec lvl _ (MkDefn _ x t) =
    hang (pPrintPrec lvl 0 x <+> equals) 2 (pPrintPrec lvl 0 t)
    -- Lam _ bs t ->
    --   let lhs = pPrintPrec lvl 0 (varName x) <+> hsep (map (pPrintPrec lvl 1) bs)
    --   in  hang (lhs <+> equals) 2 (pPrintPrec lvl 0 t)

-- instance (Pretty (TypeConOf stage), Pretty (TermConOf stage)) => Pretty (Patn stage a) where
--   pPrintPrec lvl prec patn = case patn of
--     Wild{}             -> "_"
--     Bind{_ident}       -> pretty _ident
--     Dest{_con, _patns} ->
--       maybeParens (prec > 0 && not (null _patns)) $ hsep $
--       pretty _con : map (pPrintPrec lvl 1) _patns

prettyBinds :: Vector n Bind -> Doc
prettyBinds = hsep . map pretty . toList

instance IsVar v => Pretty (Altn v) where
  pPrintPrec lvl _ (MkAltn _ p t) =
    hang ("|" <+> pPrintPrec lvl 0 p <+> "->") 2 (pPrintPrec lvl 0 t)

instance Pretty TopLevel where
  pPrintPrec _ _ = \case
    Def _ x bs t ->
      "let" <+> hang (pretty x <+> prettyBinds bs <+> equals) 2 (pretty t)
    Caf _ x t ->
      "let" <+> hang (pretty x <+> equals) 2 (pretty t)
    Asm _ x s ->
      hsep ["external", pretty x, equals, text (show s)]
