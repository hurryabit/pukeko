module CoreLang.Language.Syntax where

import Data.List (intersperse)  
import Text.PrettyPrint

type Identifier = String

data IsRec = NonRecursive | Recursive
  deriving (Show)

-- Expressions
data GenExpr a
  = Var Identifier
  | Num Integer
  | Pack Int Int
  | Ap (GenExpr a) (GenExpr a)
  | Let IsRec [GenLocalDefinition a] (GenExpr a)
  -- | Case (GenExpr a) [GenAlter a]
  | Lam [a] (GenExpr a)

type Expr = GenExpr Identifier

-- Alternatives of case expressions
-- type GenAlter a = (Integer, [a], GenExpr a)

-- type Alter = GenAlter Identifier

-- Local definitions
type GenLocalDefinition a = (a, GenExpr a)

type LocalDefinition = GenLocalDefinition Identifier

-- Definitions
type GenDefinition a = (Identifier, [a], GenExpr a)

type Definition = GenDefinition Identifier

-- Full programs
type GenProgram a = [GenDefinition a]

type Program = GenProgram Identifier


ppExpr :: Expr -> Doc
ppExpr e =
  case e of
    Var x -> text x
    Num n -> integer n
    Pack t n -> text "Pack" <> braces (hcat [int t, comma, int n])
    Ap _ _ -> parens (hsep . map ppExpr $ collect e [])
    Let r ds e0 ->
      hsep
        [ text $ case r of { NonRecursive -> "let"; Recursive -> "letrec" }
        , hsep $ intersperse (text "and") [ hsep [text x, equals, ppExpr ei]| (x, ei) <- ds ]
        , text "in"
        , ppExpr e0
        ]
    Lam xs e0 ->
      parens $ hsep
        [ text "fun"
        , hsep $ map text xs
        , text "->"
        , ppExpr e0
        ]
  where
    collect e fs =
      case e of
        Ap e1 e2 -> collect e1 (e2:fs)
        _        -> e:fs

instance Show Expr where
  show e = renderStyle (style { mode = OneLineMode }) (ppExpr e)
