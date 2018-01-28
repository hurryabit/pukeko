g_declare_cafs gm$cons_0_0, dict$Ord$Int, dict$Ring$Int, dict$Foldable$List, dict$Monad$IO, main
g_declare_main main

g_globstart gm$cons_0_0, 0
g_updcons 0, 0, 1
g_return

g_globstart gm$cons_0_2, 2
g_updcons 0, 2, 1
g_return

g_globstart gm$cons_0_4, 4
g_updcons 0, 4, 1
g_return

g_globstart gm$cons_1_2, 2
g_updcons 1, 2, 1
g_return

g_globstart op$le, 1
g_push 0
g_eval
g_uncons 4
g_pop 1
g_update 4
g_pop 3
g_unwind

g_globstart op$m, 1
g_push 0
g_eval
g_uncons 4
g_pop 2
g_update 3
g_pop 2
g_unwind

g_globstart op$t, 1
g_push 0
g_eval
g_uncons 4
g_pop 3
g_update 2
g_pop 1
g_unwind

g_globstart gm$lt, 2
g_push 1
g_eval
g_push 1
g_eval
g_les
g_update 3
g_pop 2
g_return

g_globstart gm$le, 2
g_push 1
g_eval
g_push 1
g_eval
g_leq
g_update 3
g_pop 2
g_return

g_globstart gm$ge, 2
g_push 1
g_eval
g_push 1
g_eval
g_geq
g_update 3
g_pop 2
g_return

g_globstart gm$gt, 2
g_push 1
g_eval
g_push 1
g_eval
g_gtr
g_update 3
g_pop 2
g_return

g_globstart dict$Ord$Int, 0
g_pushglobal gm$lt, 2
g_pushglobal gm$le, 2
g_pushglobal gm$ge, 2
g_pushglobal gm$gt, 2
g_push 0
g_push 2
g_push 4
g_push 6
g_updcons 0, 4, 5
g_pop 4
g_return

g_globstart gm$neg, 1
g_eval
g_neg
g_update 1
g_return

g_globstart gm$add, 2
g_push 1
g_eval
g_push 1
g_eval
g_add
g_update 3
g_pop 2
g_return

g_globstart gm$sub, 2
g_push 1
g_eval
g_push 1
g_eval
g_sub
g_update 3
g_pop 2
g_return

g_globstart gm$mul, 2
g_push 1
g_eval
g_push 1
g_eval
g_mul
g_update 3
g_pop 2
g_return

g_globstart dict$Ring$Int, 0
g_pushglobal gm$neg, 1
g_pushglobal gm$add, 2
g_pushglobal gm$sub, 2
g_pushglobal gm$mul, 2
g_push 0
g_push 2
g_push 4
g_push 6
g_updcons 0, 4, 5
g_pop 4
g_return

g_globstart gm$mod, 2
g_push 1
g_eval
g_push 1
g_eval
g_mod
g_update 3
g_pop 2
g_return

g_globstart foldr, 1
g_push 0
g_eval
g_uncons 2
g_update 3
g_pop 2
g_unwind

g_globstart foldl, 1
g_push 0
g_eval
g_uncons 2
g_pop 1
g_update 2
g_pop 1
g_unwind

g_globstart dict$Foldable$List$ll1, 3
g_push 2
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 2
g_update 2
g_pop 1
g_unwind
g_label .1
g_uncons 2
g_push 1
g_push 4
g_push 4
g_pushglobal dict$Foldable$List, 0
g_pushglobal foldr, 1
g_mkap 1
g_mkap 3
g_push 1
g_push 4
g_updap 2, 6
g_pop 5
g_unwind
g_label .2

g_globstart dict$Foldable$List$ll2, 3
g_push 2
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 2
g_update 2
g_pop 1
g_unwind
g_label .1
g_uncons 2
g_push 1
g_push 1
g_push 5
g_push 5
g_mkap 2
g_push 4
g_pushglobal dict$Foldable$List, 0
g_pushglobal foldl, 1
g_mkap 1
g_updap 3, 6
g_pop 5
g_unwind
g_label .2

g_globstart dict$Foldable$List, 0
g_pushglobal dict$Foldable$List$ll1, 3
g_pushglobal dict$Foldable$List$ll2, 3
g_push 0
g_push 2
g_updcons 0, 2, 3
g_pop 2
g_return

g_globstart take, 2
g_pushint 0
g_push 1
g_pushglobal dict$Ord$Int, 0
g_pushglobal op$le, 1
g_mkap 1
g_mkap 2
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_push 1
g_eval
g_jumpcase .3, .4
g_label .3
g_pop 1
g_pushglobal gm$cons_0_0, 0
g_update 3
g_pop 2
g_unwind
g_label .4
g_uncons 2
g_push 1
g_pushint 1
g_push 4
g_pushglobal dict$Ring$Int, 0
g_pushglobal op$m, 1
g_mkap 1
g_mkap 2
g_pushglobal take, 2
g_mkap 2
g_push 1
g_updcons 1, 2, 5
g_pop 4
g_return
g_jump .5
g_label .5
g_jump .2
g_label .1
g_pop 1
g_pushglobal gm$cons_0_0, 0
g_update 3
g_pop 2
g_unwind
g_label .2

g_globstart pure, 1
g_push 0
g_eval
g_uncons 2
g_update 3
g_pop 2
g_unwind

g_globstart op$gge, 1
g_push 0
g_eval
g_uncons 2
g_pop 1
g_update 2
g_pop 1
g_unwind

g_globstart op$s$ll1, 2
g_update 2
g_pop 1
g_unwind

g_globstart op$s, 3
g_push 2
g_pushglobal op$s$ll1, 2
g_mkap 1
g_push 2
g_push 2
g_pushglobal op$gge, 1
g_mkap 1
g_updap 2, 4
g_pop 3
g_unwind

g_globstart traverse_$ll1, 4
g_push 3
g_push 3
g_push 3
g_mkap 1
g_push 2
g_pushglobal op$s, 3
g_mkap 1
g_updap 2, 5
g_pop 4
g_unwind

g_globstart traverse_, 3
g_pushglobal gm$cons_0_0, 0
g_push 1
g_pushglobal pure, 1
g_mkap 1
g_mkap 1
g_push 3
g_push 2
g_pushglobal traverse_$ll1, 4
g_mkap 2
g_push 3
g_pushglobal foldr, 1
g_mkap 1
g_updap 2, 4
g_pop 3
g_unwind

g_globstart gm$return, 2
g_updcons 0, 2, 1
g_return

g_globstart gm$bind, 3
g_push 2
g_push 1
g_mkap 1
g_eval
g_uncons 2
g_push 3
g_updap 2, 4
g_pop 3
g_unwind

g_globstart dict$Monad$IO, 0
g_pushglobal gm$return, 2
g_pushglobal gm$bind, 3
g_push 0
g_push 2
g_updcons 0, 2, 3
g_pop 2
g_return

g_globstart gm$print, 2
g_eval
g_print
g_cons 0, 0
g_updcons 0, 2, 1
g_return

g_globstart gm$input, 1
g_input
g_updcons 0, 2, 1
g_return

g_globstart gen, 2
g_push 1
g_push 1
g_mkap 1
g_push 1
g_pushglobal gen, 2
g_mkap 2
g_push 2
g_updcons 1, 2, 3
g_pop 2
g_return

g_globstart main$ll1, 1
g_pushint 1000000007
g_push 1
g_pushint 91
g_pushglobal dict$Ring$Int, 0
g_pushglobal op$t, 1
g_mkap 1
g_mkap 2
g_eval
g_mod
g_update 2
g_pop 1
g_return

g_globstart main$ll2, 1
g_pushint 1
g_pushglobal main$ll1, 1
g_pushglobal gen, 2
g_mkap 2
g_push 1
g_pushglobal take, 2
g_mkap 2
g_pushglobal gm$print, 2
g_pushglobal dict$Foldable$List, 0
g_pushglobal dict$Monad$IO, 0
g_pushglobal traverse_, 3
g_mkap 2
g_mkap 2
g_push 1
g_pushglobal gm$print, 2
g_mkap 1
g_pushglobal dict$Monad$IO, 0
g_pushglobal op$s, 3
g_mkap 1
g_updap 2, 2
g_pop 1
g_unwind

g_globstart main, 0
g_pushglobal main$ll2, 1
g_pushglobal gm$input, 1
g_pushglobal dict$Monad$IO, 0
g_pushglobal op$gge, 1
g_mkap 1
g_updap 2, 1
g_unwind
