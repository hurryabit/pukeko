module Pukeko.GMachine.PeepHole
  ( optimize
  )
  where

import Pukeko.GMachine.GCode

optimize :: Program -> Program
optimize prog =
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
 UNWIND:JUMP _:code     -> Just (UNWIND:code)
 POP 0:code             -> Just code
 MKAP 0:code            -> Just code
 MKAP a:UPDATE k:code   -> Just (UPDAP a k:code)
 CONS t a:UPDATE k:code -> Just (UPDCONS t a k:code)
 _                      -> Nothing
