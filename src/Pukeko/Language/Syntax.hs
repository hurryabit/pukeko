{-# LANGUAGE DeriveFunctor #-}
module Pukeko.Language.Syntax
  ( Module
  , TopLevel (..)
  , Expr (..)
  , Bind (..)
  , Defn (..)
  , Altn (..)
  , mkAp
  , desugarApOp
  , desugarIf
  , unzipBinds
  , unzipDefns
  , unzipDefns3
  , Annot (..)
  , module Pukeko.Language.Ident
  )
  where

import Pukeko.Language.ADT
import Pukeko.Language.Ident
import Pukeko.Language.Operator (Spec (..), Assoc (..), aprec)
import Pukeko.Language.Type (Type, Closed)
import Pukeko.Pretty

import qualified Pukeko.Language.Operator as Operator

type Module a = [TopLevel a]

data TopLevel a
  = Type{ _annot :: a, _adts :: [ADT] }
  | Val{ _annot :: a, _ident :: Ident, _type :: Type Closed }
  | Def{ _annot :: a, _isrec :: Bool, _defns :: [Defn a] }
  | Asm{ _annot :: a, _ident :: Ident }

data Expr a
  = Var    { _annot :: a, _ident :: Ident }
  | Num    { _annot :: a, _int   :: Int }
  | Ap     { _annot :: a, _fun   :: Expr a, _args :: [Expr a] }
  | ApOp   { _annot :: a, _op    :: Ident, _arg1 :: Expr a, _arg2 :: Expr a }
  | Let    { _annot :: a, _isrec :: Bool, _defns :: [Defn a], _body :: Expr a }
  | Lam    { _annot :: a, _binds :: [Bind a], _body :: Expr a }
  | If     { _annot :: a, _cond  :: Expr a, _then  :: Expr a, _else :: Expr a }
  | Match  { _annot :: a, _expr  :: Expr a, _altns :: [Altn a] }
  deriving (Show, Functor)

data Bind a =
  MkBind { _annot :: a, _ident :: Ident, _type :: Maybe (Type Closed) }
  deriving (Show, Functor)

data Defn a = MkDefn { _bind :: Bind a, _expr :: Expr a }
  deriving (Show, Functor)

data Altn a =
  MkAltn { _annot :: a, _cons :: Ident, _binds :: [Bind a], _rhs :: Expr a }
  deriving (Show, Functor)

mkAp :: a -> Expr a -> [Expr a] -> Expr a
mkAp _annot _fun _args
  | null _args = _fun
  | otherwise  = Ap { _annot, _fun, _args }

desugarApOp :: Expr a -> Expr a
desugarApOp ApOp{ _annot, _op, _arg1, _arg2 } =
  Ap { _annot
     , _fun  = Var { _annot, _ident = _op }
     , _args = [_arg1, _arg2]
     }
desugarApOp _ = error "desugarApOp can only be applied to ApOp nodes"

desugarIf :: Expr a -> Expr a
desugarIf If{ _annot, _cond, _then, _else } =
  Match { _annot = _annot
        , _expr  = _cond
        , _altns =
          [ MkAltn { _annot = annot _then
                   , _cons  = MkIdent "True"
                   , _binds = []
                   , _rhs   = _then
                   }
          , MkAltn { _annot = annot _else
                   , _cons  = MkIdent "False"
                   , _binds = []
                   , _rhs   = _else
                   }
          ]
        }
desugarIf _ = error "desugarIf can only be applied to If nodes"

unzipBinds :: [Bind a] -> ([Ident], [Maybe (Type Closed)])
unzipBinds = unzip . map (\MkBind{ _ident, _type} -> (_ident, _type))

unzipDefns :: [Defn a] -> ([Bind a], [Expr a])
unzipDefns = unzip . map (\MkDefn{ _bind, _expr} -> (_bind, _expr))

unzipDefns3 :: [Defn a] -> ([Ident], [Maybe (Type Closed)], [Expr a])
unzipDefns3 = unzip3 .
  map (\MkDefn{ _bind = MkBind{ _ident, _type }, _expr } -> (_ident, _type, _expr))

class Annot f where
  annot :: f a -> a


instance Pretty (Expr a) where
  pPrintPrec lvl prec expr =
    case expr of
      Var  { _ident } -> pretty _ident
      Num  { _int   } -> int _int
      Ap   { _fun, _args } ->
        maybeParens (prec > aprec) $ hsep $
          pPrintPrec lvl aprec _fun : map (pPrintPrec lvl (aprec+1)) _args
      ApOp   { _op, _arg1, _arg2 } ->
        let MkSpec { _sym, _prec, _assoc } = Operator.findByName _op
            (prec1, prec2) =
              case _assoc of
                AssocLeft  -> (_prec  , _prec+1)
                AssocRight -> (_prec+1, _prec  )
                AssocNone  -> (_prec+1, _prec+1)
        in  maybeParens (prec > _prec) $
              pPrintPrec lvl prec1 _arg1 <> text _sym <> pPrintPrec lvl prec2 _arg2
      Let    { _isrec, _defns, _body  } ->
        case _defns of
          [] -> pPrintPrec lvl 0 _body
          defn0:defns -> vcat
            [ sep
              [ vcat $
                (text (if _isrec then "let rec" else "let") <+> pPrintPrec lvl 0 defn0) :
                map (\defn -> text "and" <+> pPrintPrec lvl 0 defn) defns
              , text "in"
              ]
            , pPrintPrec lvl 0 _body
            ]
      Lam    { _binds, _body  } ->
        maybeParens (prec > 0) $ hsep
          [ text "fun", hsep (map (pPrintPrec lvl 1) _binds)
          , text "->" , pPrintPrec lvl 0 _body
          ]
      If { _cond, _then, _else } ->
        maybeParens (prec > 0) $ sep
          [ text "if"  <+> pPrintPrec lvl 0 _cond <+> text "then"
          , nest 2 (pPrintPrec lvl 0 _then)
          , text "else"
          , nest 2 (pPrintPrec lvl 0 _else)
          ]
      Match { _expr, _altns } ->
        maybeParens (prec > 0) $ vcat $
        (text "match" <+> pPrintPrec lvl 0 _expr <+> text "with") :
        map (pPrintPrec lvl 0) _altns

instance Pretty (Defn a) where
  pPrintPrec lvl _ MkDefn{ _bind, _expr } =
    case _expr of
      Lam { _binds, _body } ->
        let lhs = pPrintPrec lvl 0 _bind <+> hsep (map (pPrintPrec lvl 1) _binds)
        in  hang (lhs <+> equals) 2 (pPrintPrec lvl 0 _body)
      _ -> hang (pPrintPrec lvl 0 _bind <+> equals) 2 (pPrintPrec lvl 0 _expr)

instance Pretty (Bind a) where
  pPrintPrec _ prec MkBind{ _ident, _type } =
    case _type of
      Nothing -> pretty _ident
      Just t  -> maybeParens (prec > 0) $ pretty _ident <> colon <+> pretty t

instance Pretty (Altn a) where
  pPrintPrec lvl _ MkAltn{ _cons, _binds, _rhs } = hang
    (hsep [text "|", pretty _cons, hsep (map (pPrintPrec lvl 1) _binds), text "->"]) 2
    (pPrintPrec lvl 0 _rhs)

instance Annot TopLevel where
  annot = _annot :: TopLevel _ -> _

instance Annot Expr where
  annot = _annot :: Expr _ -> _

instance Annot Bind where
  annot = _annot :: Bind _ -> _

instance Annot Altn where
  annot = _annot :: Altn _ -> _
