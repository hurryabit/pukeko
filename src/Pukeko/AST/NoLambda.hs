module Pukeko.AST.NoLambda
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

import Pukeko.Prelude

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
  pretty (MkName name) = pretty name

prettyBind :: Maybe Name -> Doc ann
prettyBind = maybe "_" pretty

prettyAltn :: Int -> Altn -> Doc ann
prettyAltn tag MkAltn{_binds, _rhs} = hang
    (hsep ["|", braces (pretty tag), hsep (map prettyBind _binds), "->"])
    2
    (pretty _rhs)

instance Pretty Expr where
  pretty = prettyPrec 0

instance PrettyPrec Expr where
  prettyPrec prec expr =
    case expr of
      Local{_name} -> pretty _name
      Global{_name} -> "@" <> pretty _name
      External{_name} -> "$" <> pretty _name
      Pack{ _tag, _arity } -> "Pack" <> braces (pretty _tag <> "," <> pretty _arity)
      Num{ _int } -> pretty _int
      Ap{ _fun, _args } ->
        maybeParens (prec > 0) $ hsep $
          prettyPrec 1 _fun : map (prettyPrec 1) _args
      Let{ _isrec, _defns, _body } ->
        case _defns of
          [] -> bug "empty let"
          defn0:defns ->
            let let_ | _isrec    = "let rec"
                     | otherwise = "let"
            in  vcat
                [ sep
                  [ vcat $
                    (let_ <+> pretty defn0) :
                    map (\defn -> "and" <+> pretty defn) defns
                  , "in"
                  ]
                , prettyPrec 0 _body
                ]
      Match { _expr, _altns } ->
        maybeParens (prec > 0) $ vcat $
        ("match" <+> prettyPrec 0 _expr <+> "with") :
        zipWith prettyAltn [0..] _altns

instance Pretty Defn where
  pretty MkDefn{_lhs, _rhs} = hang (pretty _lhs <+> "=") 2 (pretty _rhs)

instance Pretty TopLevel where
  pretty top = case top of
    Def{_name, _binds, _body} -> hang
      (hsep [ "let"
            , pretty _name
            , hsep (map prettyBind _binds)
            , "="
            ])
      2
      (prettyPrec 0 _body)
    Asm{_name} -> "asm" <+> pretty _name

instance Show Name where
  show (MkName x) = x
