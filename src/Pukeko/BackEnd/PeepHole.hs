module Pukeko.BackEnd.PeepHole
  ( optimize
  )
  where

import Pukeko.BackEnd.GCode

optimize :: Program -> Program
optimize prog = iterate optimize1 prog !! 3

optimize1 :: Program -> Program
optimize1 prog =
  let optGlobal global =
        global{ _code = globalize opt (_code global) }
  in  prog{ _globals = map optGlobal (_globals prog) }

globalize :: ([Inst] -> Maybe [Inst]) -> [Inst] -> [Inst]
globalize _ [] = []
globalize opt code@(inst:code') =
  case opt code of
    Just code' -> globalize opt code'
    Nothing    -> inst : globalize opt code'

opt :: [Inst] -> Maybe [Inst]
opt code0 = case code0 of
 UNWIND:JUMP _:code      -> Just $ UNWIND:code
 POP 0:code              -> Just code
 MKAP 0:code             -> Just code
 MKAP a:UPDATE k:code    -> Just $ UPDAP a k:code
 CONS t a:UPDATE k:code  -> Just $ UPDCONS t a k:code
 PUSH k:UPDATE l:POP m:code
  | l >= k+2 && l == m+1 -> Just $ POP k:UPDATE (l-1-k):POP (l-2-k):code
 UNCONS 0:code           -> Just $ POP 1:code
 POP k:POP l:code        -> Just $ POP (k+l):code
 _                       -> Nothing
