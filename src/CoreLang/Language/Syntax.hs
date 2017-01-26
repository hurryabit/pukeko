module CoreLang.Language.Syntax where

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
