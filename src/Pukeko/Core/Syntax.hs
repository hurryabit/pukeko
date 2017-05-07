module Pukeko.Core.Syntax
  ( Name (..)
  , Module
  , TopLevel (..)
  , Expr (..)
  , Defn (..)
  , Altn (..)
  , mkAp
  , unzipDefns
  , zipMaybe
  )
where

import Data.Maybe (catMaybes)

import Pukeko.Error
import Pukeko.Pretty

newtype Name = MkName{unName :: String}
  deriving (Eq, Ord)

data Expr
  = Local   {_name :: Name}
  | Global  {_name :: Name}
  | External{_name :: Name}
  | Pack    {_tag, _arity :: Int}
  | Num     {_int :: Int}
  | Ap      {_fun :: Expr, _args :: [Expr]}
  | Let     {_isrec :: Bool, _defns :: [Defn], _body :: Expr}
  | Match   {_expr  :: Expr, _altns :: [Altn]}
  deriving (Show)

data Defn = MkDefn{_lhs :: Name, _rhs :: Expr}
  deriving (Show)

data Altn = MkAltn{_binds :: [Maybe Name], _rhs :: Expr}
  deriving (Show)

data TopLevel
  = Def{ _name :: Name, _binds :: [Maybe Name], _body :: Expr}
  | Asm{ _name :: Name}
  deriving (Show)

type Module = [TopLevel]

mkAp :: Expr -> [Expr] -> Expr
mkAp expr []    = expr
mkAp _fun _args = Ap{_fun, _args}

unzipDefns :: [Defn] -> ([Name], [Expr])
unzipDefns = unzip . map (\MkDefn{_lhs,_rhs} -> (_lhs, _rhs))

zipMaybe :: [Maybe a] -> [b] -> [(a, b)]
zipMaybe idents = catMaybes . zipWith (\ident x -> (,) <$> ident <*> pure x) idents

instance Pretty Name where
  pPrint (MkName name) = text name

instance Show Name where
  show = prettyShow

pPrintBind :: Maybe Name -> Doc
pPrintBind = maybe (char '_') pPrint

pPrintAltn :: Int -> Altn -> Doc
pPrintAltn tag MkAltn{_binds, _rhs} = hang
    (hsep
      [ text "|"
      , braces (int tag)
      , hsep (map pPrintBind _binds)
      , text "->"
      ])
    2
    (pPrint _rhs)

instance Pretty Expr where
  pPrintPrec lvl prec expr =
    case expr of
      Local{_name} -> pPrintPrec lvl prec _name
      Global{_name} -> char '@' <> pPrintPrec lvl prec _name
      External{_name} -> char '$' <> pPrintPrec lvl prec _name
      Pack{ _tag, _arity } -> text "Pack" <> braces (int _tag <> comma <> int _arity)
      Num{ _int } -> int _int
      Ap{ _fun, _args } ->
        maybeParens (prec > 0) $ hsep $
          pPrintPrec lvl 1 _fun : map (pPrintPrec lvl 1) _args
      Let{ _isrec, _defns, _body } ->
        case _defns of
          [] -> bug "core printer" "empty let" Nothing
          defn0:defns ->
            let let_ | _isrec    = text "let rec"
                     | otherwise = text "let"
            in  vcat
                [ sep
                  [ vcat $
                    (let_ <+> pPrintPrec lvl 0 defn0) :
                    map (\defn -> text "and" <+> pPrintPrec lvl 0 defn) defns
                  , text "in"
                  ]
                , pPrintPrec lvl 0 _body
                ]
      Match { _expr, _altns } ->
        maybeParens (prec > 0) $ vcat $
        (text "match" <+> pPrintPrec lvl 0 _expr <+> text "with") :
        zipWith pPrintAltn [0..] _altns

instance Pretty Defn where
  pPrintPrec lvl _ MkDefn{_lhs, _rhs} =
    hang (pPrintPrec lvl 0 _lhs <+> equals) 2 (pPrintPrec lvl 0 _rhs)

instance Pretty TopLevel where
  pPrintPrec lvl _ top = case top of
    Def{_name, _binds, _body} -> hang
      (hsep [ text "let"
            , pPrintPrec lvl 0 _name
            , hsep (map pPrintBind _binds)
            , equals
            ])
      2
      (pPrintPrec lvl 0 _body)
    Asm{_name} -> text "asm" <+> pPrintPrec lvl 0 _name
