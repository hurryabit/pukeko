module Pukeko.BackEnd.NASM
  ( assemble
  )
where

import Pukeko.Prelude

import Data.List (isPrefixOf, intercalate)

import Pukeko.BackEnd.GCode

assemble :: Member (Error Failure) effs => Program -> Eff effs String
assemble MkProgram { _globals, _main } = do
  let funs = [ (_name, _arity) | MkGlobal{_name, _arity} <- _globals ]
      prolog = unlines
        [ "g_declare_globals " ++ intercalate ", " (map (\(f, k) -> unName f ++ ", " ++ show k) funs)
        , "g_declare_main " ++ unName _main
        ]
  globals <- traverse assembleGlobal _globals
  return $ intercalate "\n" (prolog:globals)

assembleGlobal :: (Member (Error Failure) effs) => Global -> Eff effs String
assembleGlobal MkGlobal{ _code } = unlines <$> traverse assembleInst _code

assembleInst :: (Member (Error Failure) effs) => Inst -> Eff effs String
assembleInst inst = do
  let code macro params = do
        let param_str
              | null params = ""
              | otherwise   = " " ++ intercalate ", " params
        return $ "g_" ++ macro ++ param_str
      check_label label = do
        let s = show label
        if "." `isPrefixOf` s
          then return ()
          else throwFailure ("invalid jump label:" <+> pretty label)
  case inst of
    EVAL   -> code "eval"   []
    UNWIND -> code "unwind" []
    RETURN -> code "return" []
    JUMP label -> do
      check_label label
      code "jump" [show label]
    JUMPZERO label -> do
      check_label label
      code "jumpzero" [show label]
    JUMPCASE labels -> do
      mapM_ check_label labels
      code "jumpcase" (map show labels)
    LABEL label -> do
      check_label label
      code "label" [show label]
    PUSH k -> code "push" [show k]
    PUSHINT num -> code "pushint" [show num]
    PUSHGLOBAL name -> code "pushglobal" [show name]
    GLOBSTART name arity ->
      code "globstart" [show name, show arity]
    POP k -> code "pop" [show k]
    SLIDE k -> code "slide" [show k]
    UPDATE k -> code "update" [show k]
    ALLOC k -> code "alloc" [show k]
    MKAP arity -> code "mkap" [show arity]
    UPDAP arity offset -> code "updap" [show arity, show offset]
    CONS tag arity -> code "cons" [show tag, show arity]
    UPDCONS tag arity offset -> code "updcons" [show tag, show arity, show offset]
    UNCONS arity -> code "uncons" [show arity]
    PROJ idx -> code "proj" [show idx]
    NEG -> code "neg" []
    ADD -> code "add" []
    SUB -> code "sub" []
    MUL -> code "mul" []
    DIV -> code "div" []
    MOD -> code "mod" []
    LES -> code "les" []
    LEQ -> code "leq" []
    EQV -> code "eqv" []
    NEQ -> code "neq" []
    GEQ -> code "geq" []
    GTR -> code "gtr" []
    CHR -> code "chr" []
    ORD -> code "ord" []
    PRINT -> code "print" []
    INPUT -> code "input" []
    PUTC -> code "putc" []
    GETC -> code "getc" []
    ABORT -> code "abort" []
    EXIT -> throwFailure "forbidden instruction 'EXIT'"
