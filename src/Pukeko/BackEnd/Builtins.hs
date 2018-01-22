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
externalName (MkName name) = MkName ("gm$" ++ name)

constructorName :: Int -> Int -> Name
constructorName tag arity = MkName ("gm$cons_" ++ show tag ++ "_" ++ show arity)

findGlobal :: Name -> Either Doc Global
findGlobal name = case Map.lookup (externalName name) globalTable of
  Nothing -> throwError ("unknown external:" <+> pretty name)
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
  neg : map fst binops ++ [return_, print_, input, bind, abort]

-- TODO: Produce more efficient code in Redex mode.
neg :: Global
neg = mkBuiltin "neg" 1
  [ EVAL
  , NEG
  , UPDATE 1
  , RETURN
  ]

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

return_, print_, input, bind :: Global
return_ = mkBuiltin "return" 2
  [ CONS 0 2
  , UPDATE 1
  , RETURN
  ]
print_ = mkBuiltin "print" 2
  [ EVAL
  , PRINT
  , CONS 0 0 -- unit
  , CONS 0 2 -- pair
  , UPDATE 1
  , RETURN
  ]
input = mkBuiltin "input" 1
  [ INPUT
  , CONS 0 2 -- pair
  , UPDATE 1
  , RETURN
  ]
bind = mkBuiltin "bind" 3
  [ PUSH 2
  , PUSH 1
  , MKAP 1
  , EVAL
  , UNCONS 2
  , PUSH 3
  , MKAP 2
  , UPDATE 4
  , POP 3
  , UNWIND
  ]

abort :: Global
abort = mkBuiltin "abort" 0 [ABORT]

inlineTable :: Map Name (Inst, Int)
inlineTable =
  Map.fromList $
  map (\(MkGlobal{_name, _arity}, inst) -> (_name, (inst, _arity))) binops

findInline :: Name -> Maybe (Inst, Int)
findInline name = Map.lookup (externalName name) inlineTable
