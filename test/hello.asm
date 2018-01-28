g_declare_cafs gm$cons_0_0, dict$Foldable$List, dict$Monad$IO, hello, main
g_declare_main main

g_globstart gm$cons_0_0, 0
g_updcons 0, 0, 1
g_return

g_globstart gm$cons_0_2, 2
g_updcons 0, 2, 1
g_return

g_globstart gm$cons_1_2, 2
g_updcons 1, 2, 1
g_return

g_globstart gm$chr, 1
g_eval
g_chr
g_update 1
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

g_globstart gm$putc, 2
g_eval
g_putc
g_cons 0, 0
g_updcons 0, 2, 1
g_return

g_globstart hello, 0
g_pushglobal gm$cons_0_0, 0
g_pushint 10
g_cons 1, 2
g_pushint 33
g_cons 1, 2
g_pushint 100
g_cons 1, 2
g_pushint 108
g_cons 1, 2
g_pushint 114
g_cons 1, 2
g_pushint 111
g_cons 1, 2
g_pushint 87
g_cons 1, 2
g_pushint 32
g_cons 1, 2
g_pushint 111
g_cons 1, 2
g_pushint 108
g_cons 1, 2
g_pushint 108
g_cons 1, 2
g_pushint 101
g_cons 1, 2
g_pushint 72
g_updcons 1, 2, 1
g_return

g_globstart main$ll1, 1
g_push 0
g_pushglobal gm$chr, 1
g_mkap 1
g_pushglobal gm$putc, 2
g_updap 1, 2
g_pop 1
g_unwind

g_globstart main, 0
g_pushglobal hello, 0
g_pushglobal main$ll1, 1
g_pushglobal dict$Foldable$List, 0
g_pushglobal dict$Monad$IO, 0
g_pushglobal traverse_, 3
g_mkap 2
g_updap 2, 1
g_unwind
