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
import CoreLang.Language.Operator (Spec (..), Assoc (..), aprec)
import CoreLang.Language.Term
import CoreLang.Language.Type (Type)
import CoreLang.Pretty

import qualified CoreLang.Language.Operator as Operator

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
unzipPatns = unzip . map (\MkPatn{ _ident, _type} -> (_ident, _type))

unzipDefns :: [Defn a] -> ([Patn a], [Expr a])
unzipDefns = unzip . map (\MkDefn{ _patn, _expr} -> (_patn, _expr))


class Annot f where
  annot :: f a -> a


instance Pretty (Expr a) where
  pPrintPrec lvl prec expr =
    case expr of
      Var  { _ident } -> pretty _ident
      Num  { _int   } -> int _int
      Pack { _tag, _arity } -> text "Pack" <> braces (int _tag <> comma <> int _arity)
      Ap   { _fun, _arg   } -> 
        maybeParens (prec > aprec) $
          pPrintPrec lvl aprec _fun <+> pPrintPrec lvl (aprec+1) _arg
      ApOp   { _op, _arg1, _arg2 } -> 
        let MkIdent name = _op
            MkSpec { _prec, _assoc } = Operator.find name
            (prec1, prec2) =
              case _assoc of
                AssocLeft  -> (_prec  , _prec+1)
                AssocRight -> (_prec+1, _prec  )
                AssocNone  -> (_prec+1, _prec+1)
        in  maybeParens (prec > _prec) $
              pPrintPrec lvl prec1 _arg1 <> pretty _op <> pPrintPrec lvl prec2 _arg2
      Let    { _defns, _body  } -> let_ "let"    _defns _body
      LetRec { _defns, _body  } -> let_ "letrec" _defns _body
      Lam    { _patns, _body  } ->
        maybeParens (prec > 0) $ hsep
          [ text "fun", hsep (map (pPrintPrec lvl 1) _patns)
          , text "->" , pPrintPrec lvl 0 _body
          ]
      If { _cond, _then, _else } ->
        maybeParens (prec > 0) $ hsep
          [ text "if"  , pPrintPrec lvl 0 _cond
          , text "then", pPrintPrec lvl 0 _then
          , text "else", pPrintPrec lvl 0 _else
          ]
      Rec { _defns } -> braces $ hsep $ punctuate comma (map pretty _defns)
      Sel { _expr, _field } -> pretty _expr <> char '.' <> pretty _field
    where
      let_ key defns body =
        hsep
          [ text key , hcat $ punctuate (text " and ") (map (pPrintPrec lvl 0) defns)
          , text "in", pPrintPrec lvl 0 body
          ]

instance Pretty (Defn a) where
  pPrintPrec lvl _ MkDefn{ _patn, _expr } =
    pPrintPrec lvl 0 _patn <+> equals <+> pPrintPrec lvl 0 _expr

instance Pretty (Patn a) where
  pPrintPrec _ prec MkPatn{ _ident, _type } =
    case _type of
      Nothing -> pretty _ident
      Just t  -> maybeParens (prec > 0) $ pretty _ident <> colon <+> pretty t


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
  annot MkPatn{ _annot } = _annot
