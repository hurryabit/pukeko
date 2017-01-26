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
    Ap e1 e2 -> parens (ppExpr e1 <+> ppExpr e2)
    Let r ds e0 ->
      hsep
        [ text $ case r of { NonRecursive -> "let"; Recursive -> "letrec" }
        , hsep $ intersperse (text "and") [ hsep [text x, equals, ppExpr ei]| (x, ei) <- ds ]
        , text "in"
        , ppExpr e0
        ]
    Lam xs e0 ->
      hsep
        [ text "fun"
        , hsep $ map text xs
        , text "->"
        , ppExpr e0
        ]

instance Show Expr where
  show e = renderStyle (style { mode = OneLineMode }) (ppExpr e)
