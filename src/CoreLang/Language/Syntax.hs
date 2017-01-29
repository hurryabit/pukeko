module CoreLang.Language.Syntax where

import Data.List (intersperse)  
import Text.PrettyPrint

import CoreLang.Language.Type (Type)

type Identifier = String

-- Expressions
data Expr
  = Var    Identifier
  | Num    Integer
  | Pack   Int Int
  | Ap     Expr Expr
  | Let    [Definition] Expr
  | LetRec [Definition] Expr
  | Lam    [Declaration] Expr
  | If     Expr Expr Expr

type Declaration = (Identifier, Maybe Type)

type Definition = (Declaration, Expr)


ppExpr :: Expr -> Doc
ppExpr e =
  case e of
    Var x -> text x
    Num n -> integer n
    Pack t n -> text "Pack" <> braces (hcat [int t, comma, int n])
    Ap _ _ -> parens (hsep . map ppExpr $ collect e [])
    Let    ds e0 -> ppLet "let" ds e0
    LetRec ds e0 -> ppLet "letrec" ds e0
    Lam xs e0 ->
      parens $ hsep
        [ text "fun"
        , hsep [ text x | (x, _) <- xs ]
        , text "->"
        , ppExpr e0
        ]
    If ec et ef ->
      hsep $
        [ text "if"
        , ppExpr ec
        , text "then"
        , ppExpr et
        , text "else"
        , ppExpr ef
        ]
  where
    collect e fs =
      case e of
        Ap e1 e2 -> collect e1 (e2:fs)
        _        -> e:fs

ppLet key ds e0 =
  hsep
    [ text key
    , hsep $ intersperse (text "and") [ hsep [text x, equals, ppExpr ei]| ((x, _), ei) <- ds ]
    , text "in"
    , ppExpr e0
    ]
  

instance Show Expr where
  show e = renderStyle (style { mode = OneLineMode }) (ppExpr e)
