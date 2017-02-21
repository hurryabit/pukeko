{-# LANGUAGE DeriveFunctor, DeriveTraversable #-}
module Pukeko.GMachine.GCode where

import Data.Char (isSpace)
import Data.Set (Set)

import qualified Data.Set as Set

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
  | CONS        Int Int
  | MKAP
  -- Selectors
  | HEAD
  | TAIL
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
  -- I/O
  | PRINT
  -- Jumps
  | JUMP        lab
  | JUMPZERO    lab
  | LABEL       lab
  -- Evaluation control
  | EXIT
  | ABORT
  | EVAL
  | UNWIND
  | RETURN
  deriving (Eq, Read, Show, Foldable, Functor, Traversable)

newtype Name = Name { unName :: String }
  deriving (Eq, Ord)

type Inst = GInst Name

data Global = MkGlobal
  { _name  :: Name
  , _arity :: Int
  , _code  :: [Inst]
  }

dependencies :: Global -> Set Name
dependencies MkGlobal{ _code } =
  let extend set inst =
        case inst of
          PUSHGLOBAL name -> Set.insert name set
          _               -> set
  in  foldl extend Set.empty _code

instance Show Name where
  show (Name name) = name

instance Read Name where
  readsPrec _ input =
    case break isSpace (dropWhile isSpace input) of
      ("", _)      -> []
      (name, rest) -> [(Name name, rest)]

instance Pretty Program where
  pPrint MkProgram { _globals, _main } =
    vcat $ map (text . show) $
    [ PUSHGLOBAL _main
    , EVAL
    , PRINT
    , EXIT
    ]
    ++
    concatMap _code _globals
