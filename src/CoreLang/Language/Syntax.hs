{-# LANGUAGE DeriveFunctor #-}
module CoreLang.Language.Syntax
  ( Expr (..)
  , Patn (..)
  , Defn (..)
  , unzipPatns
  , unzipDefns
  , Annot (..)
  , module CoreLang.Language.Ident
  )
  where

import CoreLang.Language.Ident
import CoreLang.Language.Term
import CoreLang.Language.Type (Type)
import CoreLang.Pretty

data Expr a
  = Var    { _annot :: a, _ident :: Ident }
  | Num    { _annot :: a, _int   :: Int   }
  | Pack   { _annot :: a, _tag   :: Int , _arity :: Int  }
  | Ap     { _annot :: a, _fun   :: Expr a, _arg :: Expr a }
  | ApOp   { _annot :: a, _op    :: Ident, _arg1 :: Expr a, _arg2 :: Expr a }
  | Let    { _annot :: a, _defns :: [Defn a], _body  :: Expr a }
  | LetRec { _annot :: a, _defns :: [Defn a], _body  :: Expr a }
  | Lam    { _annot :: a, _patns :: [Patn a], _body  :: Expr a }
  | If     { _annot :: a, _cond  :: Expr a, _then  :: Expr a, _else :: Expr a }
  | Rec    { _annot :: a, _defns :: [Defn a] }
  | Sel    { _annot :: a, _expr  :: Expr a, _field :: Ident }
  deriving (Show, Functor)

data Patn a = MkPatn { _annot :: a, _ident :: Ident, _type :: Maybe Type }
  deriving (Show, Functor)

data Defn a = MkDefn { _patn :: Patn a, _expr :: Expr a }
  deriving (Show, Functor)


unzipPatns :: [Patn a] -> ([Ident], [Maybe Type])
unzipPatns = unzip . map (\MkPatn { _ident, _type} -> (_ident, _type))

unzipDefns :: [Defn a] -> ([Patn a], [Expr a])
unzipDefns = unzip . map (\MkDefn { _patn, _expr} -> (_patn, _expr))


class Annot f where
  annot :: f a -> a


instance Pretty (Patn a) where
  pPrint MkPatn { _ident, _type } =
    case _type of
      Nothing -> pretty _ident
      Just t  -> parens $ pretty _ident <> colon <+> pretty t

instance Pretty (Defn a) where
  pPrint MkDefn { _patn, _expr } = pretty _patn <+> equals <+> pretty _expr

instance Pretty (Expr a) where
  pPrint expr =
    case expr of
      Var    { _ident } -> pretty _ident
      Num    { _int   } -> int _int
      Pack   { _tag, _arity } -> text "Pack" <> braces (int _tag <> comma <> int _arity)
      Ap     { _fun, _arg   } -> parens $ hsep $ map pretty (collect expr [])
      ApOp   { _op, _arg1, _arg2 } -> parens $ pretty _arg1 <> pretty _op <> pretty _arg2
      Let    { _defns, _body  } -> let_ "let"    _defns _body
      LetRec { _defns, _body  } -> let_ "letrec" _defns _body
      Lam    { _patns, _body  } ->
        parens $ hsep
          [ text "fun"
          , hsep (map pretty _patns)
          , text "->"
          , pretty _body
          ]
      If { _cond, _then, _else } ->
        hsep
          [ text "if"
          , pretty _cond
          , text "then"
          , pretty _then
          , text "else"
          , pretty _else
          ]
      Rec { _defns } -> braces $ hsep $ punctuate comma (map pretty _defns)
      Sel { _expr, _field } -> pretty _expr <> char '.' <> pretty _field
    where
      collect expr acc =
        case expr of
          Ap { _fun, _arg } -> collect _fun (_arg:acc)
          _                 -> expr:acc
      let_ key defns body =
        hsep
          [ text key
          , hcat $ punctuate (text " and ") (map pretty defns)
          , text "in"
          , pretty body
          ]


instance Annot Expr where
  annot expr =
    case expr of
      Var    { _annot } -> _annot
      Num    { _annot } -> _annot
      Pack   { _annot } -> _annot
      Ap     { _annot } -> _annot
      ApOp   { _annot } -> _annot
      Let    { _annot } -> _annot
      LetRec { _annot } -> _annot
      Lam    { _annot } -> _annot
      If     { _annot } -> _annot
      Rec    { _annot } -> _annot
      Sel    { _annot } -> _annot

instance Annot Patn where
  annot MkPatn { _annot } = _annot
