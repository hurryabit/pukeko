module CoreLang.GMachine.NASM where

import Control.Monad.Except
import Data.List (isPrefixOf, intercalate)
import Data.Map (Map)

import qualified Data.Map as Map

import CoreLang.GMachine.GCode

assemble :: MonadError String m => Program -> m String
assemble MkProgram { _globals, _main } = do
  let arities = Map.fromList $
        map (\MkGlobal{ _name, _arity } -> (_name, _arity)) _globals
      prolog = unlines ["%include 'runtime.inc'", "", "section .text"]
  globals <- mapM (assembleGlobal arities) _globals
  return $ intercalate "\n" (prolog:globals)

assembleGlobal :: MonadError String m => Map Name Int -> Global -> m String
assembleGlobal arities MkGlobal{ _code } =
  unlines <$> mapM (assembleInst arities) _code

assembleInst :: MonadError String m => Map Name Int -> Inst -> m String
assembleInst arities inst = do
  let code macro params = do
        let param_str
              | null params = ""
              | otherwise   = " " ++ intercalate ", " params
        return $ "  g_" ++ macro ++ param_str
      binop instr = code "binop" [instr]
      divop rgstr = code "divop" [rgstr]
      relop instr = code "relop" [instr]
      mark label = return $ show label ++ ":"
      check_dot label = do
        let s = show label
        if "." `isPrefixOf` s
          then return ()
          else throwError $ s ++ " is not a valid jump label"
  case inst of
    EVAL   -> code "eval"   []
    UNWIND -> code "unwind" []
    RETURN -> code "return" []
    JUMP label -> do
      check_dot label
      code "jump" [show label]
    JUMPZERO label -> do
      check_dot label
      code "jumpzero" [show label]
    LABEL label -> do
      check_dot label
      mark label
    PUSH k -> code "push" [show k]
    PUSHINT num -> code "pushint" [show num]
    PUSHGLOBAL name -> do
      case Map.lookup name arities of
        Nothing -> throwError $ "Unknown global: " ++ show name
        Just arity -> code "pushglobal" [show name, show arity]
    GLOBSTART name arity -> do
      m <- mark name
      c <- code "globstart" [show arity]
      return $ m ++ "\n" ++ c
    POP k -> code "pop" [show k]
    SLIDE k -> code "slide" [show k]
    UPDATE k -> code "update" [show k]
    ALLOC k -> code "alloc" [show k]
    MKAP -> code "mkap" []
    CONS tag arity -> code "cons" [show tag, show arity]
    HEAD -> code "head" []
    TAIL -> code "tail" []
    NEG -> code "neg" []
    ADD -> binop "add"
    SUB -> binop "sub"
    MUL -> binop "imul"
    DIV -> divop "rax"
    MOD -> divop "rdx"
    LES -> relop "setb"
    LEQ -> relop "setbe"
    EQV -> relop "sete"
    NEQ -> relop "setne"
    GEQ -> relop "setae"
    GTR -> relop "seta"
    PRINT -> code "print" []
    ABORT -> code "abort" []
    EXIT -> throwError "EXIT not allowed"
