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
  | Case (GenExpr a) [GenAlter a]
  | Lam [a] (GenExpr a)
  deriving (Show)

type Expr = GenExpr Identifier

-- Alternatives of case expressions
type GenAlter a = (Integer, [a], GenExpr a)

type Alter = GenAlter Identifier

-- Local definitions
type GenLocalDefinition a = (a, GenExpr a)

type LocalDefinition = GenLocalDefinition Identifier

-- Definitions
type GenDefinition a = (Identifier, [a], GenExpr a)

type Definition = GenDefinition Identifier

-- Full programs
type GenProgram a = [GenDefinition a]

type Program = GenProgram Identifier

prelude :: Program
prelude =
  [ ("I", ["x"], Var "x")
  , ("K", ["x", "y"], Var "x")
  , ("K1", ["x", "y"], Var "y")
  , ("S", ["f", "g", "x"], Ap (Ap (Var "f") (Var "x"))
                              (Ap (Var "g") (Var "x")))
  , ("compose", ["f", "g", "x"], Ap (Var "f") (Ap (Var "g") (Var "x")))
  , ("twice", ["f"], Ap (Ap (Var "compose") (Var "f")) (Var "f"))
  , ("false", [], Pack (fromEnum False) 0)
  , ("true" , [], Pack (fromEnum True ) 0)
  , ("not", ["x"], Ap (Ap (Ap (Var "if") (Var "x")) (Var "false")) (Var "true"))
  , ("&", ["x", "y"], Ap (Ap (Ap (Var "if") (Var "x")) (Var "y"))    (Var "false"))
  , ("|" , ["x", "y"], Ap (Ap (Ap (Var "if") (Var "x")) (Var "true")) (Var "y"))
  , ("xor", ["x", "y"], Ap (Ap (Ap (Var "if") (Var "x")) (Ap (Var "not") (Var "y"))) (Var "y"))
  , ("pair", [], Pack 0 2)
  , ("fst",["p"],Ap (Ap (Var "unpair") (Var "p")) (Var "K"))
  , ("snd",["p"],Ap (Ap (Var "unpair") (Var "p")) (Var "K1"))
  ]

lhss :: [(a, b)] -> [a]
lhss = map fst

rhss :: [(a, b)] -> [b]
rhss = map snd

isAtomic :: GenExpr a -> Bool
isAtomic e =
  case e of
    Var _ -> True
    Num _ -> True
    _     -> False
