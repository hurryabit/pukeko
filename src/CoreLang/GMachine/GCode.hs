{-# LANGUAGE DeriveFunctor, DeriveTraversable #-}
module CoreLang.GMachine.GCode where

import Data.Char (isSpace)


type GCode = [GInst Name]

data GInst lab
  = EVAL
  | UNWIND
  | RETURN
  | EXIT
  | JUMP        lab
  | JUMPNIL     lab
  | LABEL       lab
  | PUSH        Int
  | PUSHINT     Int
  | PUSHGLOBAL  lab
  | GLOBSTART   lab Int
  | POP         Int
  | SLIDE       Int
  | UPDATE      Int
  | ALLOC       Int
  | MKAP
  | CONS0       Int
  | CONS1       Int
  | CONS2       Int
  | HEAD
  | TAIL
  | NEG
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | LES
  | LEQ
  | EQU
  | NEQ
  | GEQ
  | GTR
  | PRINT
  deriving (Eq, Read, Show, Foldable, Functor, Traversable)

newtype Name = Name String
  deriving (Eq, Ord)

instance Show Name where
  show (Name name) = name

instance Read Name where
  readsPrec _ input =
    case break isSpace (dropWhile isSpace input) of
      ("", _)      -> []
      (name, rest) -> [(Name name, rest)]
