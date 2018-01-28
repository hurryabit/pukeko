module Pukeko.BackEnd.NASM
  ( assemble
  )
where

import Pukeko.Prelude

import Data.List (isPrefixOf, intercalate)
import qualified Data.Map as Map

import Pukeko.BackEnd.GCode

assemble :: Program -> Either Failure String
assemble MkProgram { _globals, _main } = run . runError $ do
  let arities = Map.fromList $
        map (\MkGlobal{ _name, _arity } -> (_name, _arity)) _globals
      cafs = [ _name | MkGlobal{ _name, _arity = 0 } <- _globals ]
      prolog = unlines
        [ "g_declare_cafs " ++ intercalate ", " (map unName cafs)
        , "g_declare_main " ++ unName _main
        ]
  globals <- mapM (assembleGlobal arities) _globals
  return $ intercalate "\n" (prolog:globals)

assembleGlobal ::
  (Member (Error Failure) effs) => Map Name Int -> Global -> Eff effs String
assembleGlobal arities MkGlobal{ _code } =
  unlines <$> mapM (assembleInst arities) _code

assembleInst :: (Member (Error Failure) effs) => Map Name Int -> Inst -> Eff effs String
assembleInst arities inst = do
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
    PUSHGLOBAL name -> do
      case Map.lookup name arities of
        Nothing -> throwFailure ("unknown global:" <+> pretty name)
        Just arity -> code "pushglobal" [show name, show arity]
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
