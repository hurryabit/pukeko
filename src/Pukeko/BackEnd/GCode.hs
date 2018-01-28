module Pukeko.BackEnd.GCode
  ( Name (..)
  , GInst (..)
  , Inst
  , Global (..)
  , Program (..)
  )
where

import Pukeko.Prelude

import Pukeko.AST.NoLambda (Name (..))
import Pukeko.Pretty

data Program = MkProgram
  { _globals :: [Global]
  , _main    :: Name
  }

data GInst lab
  -- Stack control
  = GLOBSTART   lab Int
  | PUSH        Int
  | POP         Int
  | SLIDE       Int
  | UPDATE      Int
  | ALLOC       Int
  -- Node constructors
  | PUSHINT     Int
  | PUSHGLOBAL  lab
  | MKAP        Int
  | UPDAP       Int Int
  | CONS        Int Int
  | UPDCONS     Int Int Int
  | UNCONS      Int
  -- Arithmetic
  | NEG
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  -- Comparison relations
  | LES
  | LEQ
  | EQV
  | NEQ
  | GEQ
  | GTR
  -- Character conversion
  | CHR
  | ORD
  -- I/O
  | PRINT
  | INPUT
  | PUTC
  | GETC
  -- Jumps
  | JUMP        lab
  | JUMPZERO    lab
  | JUMPCASE    [lab]
  | LABEL       lab
  -- Evaluation control
  | EXIT
  | ABORT
  | EVAL
  | UNWIND
  | RETURN
  deriving (Eq, Read, Show, Foldable, Functor, Traversable)

type Inst = GInst Name

data Global = MkGlobal
  { _name  :: Name
  , _arity :: Int
  , _code  :: [Inst]
  }

instance Pretty Program where
  pretty MkProgram { _globals, _main } =
    vcat $ map (pretty . show) $
    [ PUSHGLOBAL _main
    , EVAL
    , PRINT
    , EXIT
    ]
    ++
    concatMap _code _globals
