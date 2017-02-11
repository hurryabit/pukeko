{-# LANGUAGE DeriveFunctor, DeriveTraversable #-}
module CoreLang.GMachine.GCode where

import Data.Char (isSpace)

import CoreLang.Pretty

newtype GProg = GProg GCode

type GCode = [GInst Name]

data GInst lab
  = EVAL
  | UNWIND
  | RETURN
  | EXIT
  | JUMP        lab
  | JUMPZERO    lab
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
  | EQV
  | NEQ
  | GEQ
  | GTR
  | PRINT
  | ABORT
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

instance Pretty GProg where
  pPrint (GProg insts) = vcat $ map (text . show) insts
