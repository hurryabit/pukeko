module Pukeko.BackEnd.Builtins
  ( externalName
  , constructorName
  , constructor
  , findGlobal
  , findInline
  )
  where

import Pukeko.Prelude

import qualified Data.Map as Map

import Pukeko.BackEnd.GCode

externalName :: Name -> Name
externalName (MkName name) = MkName ("B." ++ name)

constructorName :: Int -> Int -> Name
constructorName tag arity = MkName ("C." ++ show tag ++ "." ++ show arity)

findGlobal :: Name -> Either Failure Global
findGlobal name = case Map.lookup (externalName name) globalTable of
  Nothing -> Left ("unknown external:" <+> pretty name)
  Just global -> return global

mkBuiltinGen :: Name -> Int -> [Inst] -> Global
mkBuiltinGen _name _arity code =
  let _code = GLOBSTART _name _arity : code
  in  MkGlobal{_name, _arity, _code}

mkBuiltin :: String -> Int -> [Inst] -> Global
mkBuiltin = mkBuiltinGen . externalName . MkName

globalTable :: Map Name Global
globalTable =
  Map.fromList $
  map (\global@MkGlobal{_name} -> (_name, global)) $
  map fst (unops ++ binops) ++ io ++ [abort]

-- TODO: Produce more efficient code in Redex mode.
unops :: [(Global, Inst)]
unops =
  [ mk "neg" NEG
  , mk "chr" CHR
  , mk "ord" ORD
  ]
  where
    mk name inst =
      let global = mkBuiltin name 1
            [ EVAL
            , inst
            , UPDATE 1
            , RETURN
            ]
      in  (global, inst)

binops :: [(Global, Inst)]
binops =
  [ mk "add" ADD
  , mk "sub" SUB
  , mk "mul" MUL
  , mk "div" DIV
  , mk "mod" MOD
  , mk "lt"  LES
  , mk "le"  LEQ
  , mk "eq"  EQV
  , mk "ne"  NEQ
  , mk "ge"  GEQ
  , mk "gt"  GTR
  ]
  where
    mk name inst =
      let global = mkBuiltin name 2
            [ PUSH 1
            , EVAL
            , PUSH 1
            , EVAL
            , inst
            , UPDATE 3
            , POP 2
            , RETURN
            ]
      in  (global, inst)

constructor :: Int -> Int -> Global
constructor tag arity = mkBuiltinGen (constructorName tag arity) arity
  [ CONS tag arity
  , UPDATE 1
  , RETURN
  ]

io :: [Global]
io =
  [ mkBuiltin "seq" 2
    [ EVAL
    , POP 1
    , UPDATE 1
    , UNWIND
    ]
  , mkBuiltin "puti" 1
    [ EVAL
    , PRINT
    , CONS 0 0 -- unit
    , UPDATE 1
    , RETURN
    ]
  , mkBuiltin "geti" 1
    [ POP 1 -- unit
    , INPUT
    , UPDATE 1
    , RETURN
    ]
  , mkBuiltin "putc" 1
    [ EVAL
    , PUTC
    , CONS 0 0 -- unit
    , UPDATE 1
    , RETURN
    ]
  , mkBuiltin "getc" 1
    [ POP 1 -- unit
    , GETC
    , UPDATE 1
    , RETURN
    ]
  ]

abort :: Global
abort = mkBuiltin "abort" 0 [ABORT]

inlineTable :: Map Name (Inst, Int)
inlineTable =
  Map.fromList $
  map (\(MkGlobal{_name, _arity}, inst) -> (_name, (inst, _arity))) (unops ++ binops)

findInline :: Name -> Maybe (Inst, Int)
findInline name = Map.lookup (externalName name) inlineTable
