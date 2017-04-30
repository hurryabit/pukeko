{-# LANGUAGE DeriveFunctor #-}
module Pukeko.Language.Syntax
  ( Module
  , TopLevel (..)
  , Expr (..)
  , BindGen (..)
  , Bind
  , Bind0
  , Defn (..)
  , Altn (..)
  , mkAp
  , mkApOp
  , desugarIf
  , unzipDefns
  , zipMaybe
  , Annot (..)
  )
where

import Data.Maybe (catMaybes)

import Pukeko.Language.ADT
import Pukeko.Language.Operator (aprec)
import Pukeko.Language.Type (Type, Closed)
import Pukeko.Pretty

import qualified Pukeko.Language.Ident    as Ident

type Module a = [TopLevel a]

data TopLevel a
  = Type{ _annot :: a, _adts :: [ADT] }
  | Val{ _annot :: a, _ident :: Ident.Var, _type :: Type Closed }
  | Def{ _annot :: a, _isrec :: Bool, _defns :: [Defn a] }
  | Asm{ _annot :: a, _ident :: Ident.Var, _asm :: String }

data Expr a
  = Var    { _annot :: a, _var   :: Ident.Var }
  | Con    { _annot :: a, _con   :: Ident.Con }
  | Num    { _annot :: a, _int   :: Int }
  | Ap     { _annot :: a, _fun   :: Expr a   , _args  :: [Expr a] }
  | Lam    { _annot :: a, _binds :: [Bind0 a], _body  :: Expr a   }
  | Let    { _annot :: a, _isrec :: Bool     , _defns :: [Defn a], _body :: Expr a }
  | If     { _annot :: a, _cond  :: Expr a   , _then  :: Expr a  , _else :: Expr a }
  | Match  { _annot :: a, _expr  :: Expr a   , _altns :: [Altn a] }
  deriving (Show, Functor)

data BindGen i a = MkBind{ _annot :: a, _ident :: i, _type :: Maybe (Type Closed) }
  deriving (Show, Functor)

type Bind = BindGen Ident.Var

type Bind0 = BindGen (Maybe Ident.Var)

data Defn a = MkDefn{ _lhs :: Bind a, _rhs :: Expr a }
  deriving (Show, Functor)

data Altn a = MkAltn{ _annot :: a, _con :: Ident.Con, _binds :: [Bind0 a], _rhs :: Expr a }
  deriving (Show, Functor)

mkAp :: a -> Expr a -> [Expr a] -> Expr a
mkAp _annot _fun _args
  | null _args = _fun
  | otherwise  = Ap { _annot, _fun, _args }

mkApOp :: String -> a -> Expr a -> Expr a -> Expr a
mkApOp sym _annot arg1 arg2 =
  let _fun = Var{ _annot, _var = Ident.operator sym }
  in  Ap{ _annot, _fun, _args = [arg1, arg2]}

desugarIf :: Expr a -> Expr a
desugarIf If{ _annot, _cond, _then, _else } =
  Match { _annot = _annot
        , _expr  = _cond
        , _altns =
          [ MkAltn { _annot = annot _else
                   , _con   = Ident.constructor "False"
                   , _binds = []
                   , _rhs   = _else
                   }
          , MkAltn { _annot = annot _then
                   , _con   = Ident.constructor "True"
                   , _binds = []
                   , _rhs   = _then
                   }
          ]
        }
desugarIf _ = error "BUG: desugarIf called on wrong node"

unzipDefns :: [Defn a] -> ([Bind a], [Expr a])
unzipDefns = unzip . map (\MkDefn{ _lhs, _rhs } -> (_lhs, _rhs))

zipMaybe :: [Maybe a] -> [b] -> [(a, b)]
zipMaybe xs = catMaybes . zipWith (\x y -> (,) <$> x <*> pure y) xs

instance Pretty (Expr a) where
  pPrintPrec lvl prec expr =
    case expr of
      Var  { _var } -> pretty _var
      Con  { _con } -> pretty _con
      Num  { _int } -> int _int
      Ap   { _fun, _args } ->
        maybeParens (prec > aprec) $ hsep $
          pPrintPrec lvl aprec _fun : map (pPrintPrec lvl (aprec+1)) _args
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
  pPrintPrec lvl _ MkDefn{ _lhs, _rhs } = case _rhs of
    Lam { _binds, _body } ->
      let lhs = pPrintPrec lvl 0 _lhs <+> hsep (map (pPrintPrec lvl 1) _binds)
      in  hang (lhs <+> equals) 2 (pPrintPrec lvl 0 _body)
    _ -> hang (pPrintPrec lvl 0 _lhs <+> equals) 2 (pPrintPrec lvl 0 _rhs)

instance Pretty (Bind a) where
  pPrintPrec _ prec MkBind{ _ident, _type } =
    let p_ident = pretty _ident
    in case _type of
         Nothing -> p_ident
         Just t  -> maybeParens (prec > 0) $ p_ident <> colon <+> pretty t

instance Pretty (Bind0 a) where
  pPrintPrec _ prec MkBind{ _ident, _type } =
    let p_ident = maybe (text "_") pretty _ident
    in case _type of
         Nothing -> p_ident
         Just t  -> maybeParens (prec > 0) $ p_ident <> colon <+> pretty t

instance Pretty (Altn a) where
  pPrintPrec lvl _ MkAltn{ _con, _binds, _rhs } = hang
    (hsep [text "|", pretty _con, hsep (map (pPrintPrec lvl 1) _binds), text "->"]) 2
    (pPrintPrec lvl 0 _rhs)

instance Pretty (TopLevel a) where
  pPrintPrec lvl _ top = case top of
    Type{} -> empty
    Val{} -> empty
    -- TODO: Avoid code duplication.
    Def{ _isrec, _defns } -> case _defns of
      [] -> empty
      defn0:defns -> vcat $
        (text (if _isrec then "let rec" else "let") <+> pPrintPrec lvl 0 defn0) :
        map (\defn -> text "and" <+> pPrintPrec lvl 0 defn) defns
    Asm{ _ident, _asm } ->
      hsep [text "external", pretty _ident, equals, text (show _asm)]

class Annot f where
  annot :: f a -> a

instance Annot TopLevel where
  annot = _annot :: TopLevel _ -> _

instance Annot Expr where
  annot = _annot :: Expr _ -> _

instance Annot (BindGen i) where
  annot = _annot :: BindGen _ _ -> _

instance Annot Altn where
  annot = _annot :: Altn _ -> _
