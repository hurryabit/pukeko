{-# LANGUAGE DeriveFunctor, DeriveTraversable #-}
module CoreLang.GMachine.GCode where

import Data.Char (isSpace)
import Data.Set (Set)

import qualified Data.Set as Set

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

newtype Name = Name { unName :: String }
  deriving (Eq, Ord)

data Global = MkGlobal
  { _name  :: Name
  , _arity :: Int
  , _code  :: GCode
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

instance Pretty GProg where
  pPrint (GProg insts) = vcat $ map (text . show) insts
