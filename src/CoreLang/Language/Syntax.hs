module CoreLang.Language.Syntax
  ( Expr (..)
  , Decl (..)
  , Defn (..)
  , unzipDecls
  , unzipDefns
  , module CoreLang.Language.Ident
  )
  where

import CoreLang.Language.Ident
import CoreLang.Language.Type (Type)
import CoreLang.Pretty

data Expr a
  = Var    { _annot :: a, _ident :: Ident }
  | Num    { _annot :: a, _int   :: Int   }
  | Pack   { _annot :: a, _tag   :: Int , _arity :: Int  }
  | Ap     { _annot :: a, _fun   :: Expr a, _arg   :: Expr a }
  | Let    { _annot :: a, _defns :: [Defn a], _body  :: Expr a }
  | LetRec { _annot :: a, _defns :: [Defn a], _body  :: Expr a }
  | Lam    { _annot :: a, _decls :: [Decl a], _body  :: Expr a }
  | If     { _annot :: a, _cond  :: Expr a, _then  :: Expr a, _else :: Expr a }
  | Rec    { _annot :: a, _defns :: [Defn a] }
  | Sel    { _annot :: a, _expr  :: Expr a, _field :: Ident }
  deriving (Show)

data Decl a = MkDecl { _annot :: a, _ident :: Ident, _type :: Maybe Type }
  deriving (Show)

data Defn  a = MkDefn { _annot :: a, _decl :: Decl a, _expr :: Expr a }
  deriving (Show)


unzipDecls :: [Decl a] -> ([Ident], [Maybe Type])
unzipDecls = unzip . map (\MkDecl { _ident, _type} -> (_ident, _type))

unzipDefns :: [Defn a] -> ([Decl a], [Expr a])
unzipDefns = unzip . map (\MkDefn { _decl, _expr} -> (_decl, _expr))


instance Pretty (Decl a) where
  pPrint (MkDecl { _ident, _type }) =
    case _type of
      Nothing -> pretty _ident
      Just t  -> parens $ pretty _ident <> colon <+> pretty t

instance Pretty (Defn a) where
  pPrint (MkDefn { _decl, _expr }) = pretty _decl <+> equals <+> pretty _expr

instance Pretty (Expr a) where
  pPrint expr =
    case expr of
      Var    { _ident } -> pretty _ident
      Num    { _int   } -> int _int
      Pack   { _tag  , _arity } -> text "Pack" <> braces (int _tag <> comma <> int _arity)
      Ap     { _fun  , _arg   } -> parens $ hsep $ map pretty (collect expr [])
      Let    { _defns, _body  } -> let_ "let"    _defns _body
      LetRec { _defns, _body  } -> let_ "letrec" _defns _body
      Lam    { _decls, _body  } ->
        parens $ hsep
          [ text "fun"
          , hsep (map pretty _decls)
          , text "->"
          , pretty _body
          ]
      If { _cond, _then, _else } ->
        hsep $
          [ text "if"
          , pretty _cond
          , text "then"
          , pretty _then
          , text "else"
          , pretty _else
          ]
      Rec { _defns } -> braces $ hsep $ punctuate (comma) (map pretty _defns)
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
  