g_declare_cafs gm$cons_0_0, main
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

g_globstart gm$sub, 2
g_push 1
g_eval
g_push 1
g_eval
g_sub
g_update 3
g_pop 2
g_return

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

g_globstart foldr, 3
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
g_pushglobal foldr, 3
g_mkap 3
g_push 1
g_push 4
g_updap 2, 6
g_pop 5
g_unwind
g_label .2

g_globstart partition$ll1, 3
g_push 2
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal gm$cons_0_0, 0
g_pushglobal gm$cons_0_0, 0
g_updcons 0, 2, 4
g_pop 3
g_return
g_jump .2
g_label .1
g_uncons 2
g_push 1
g_push 4
g_mkap 1
g_eval
g_uncons 2
g_push 2
g_push 5
g_mkap 1
g_eval
g_jumpcase .3, .4
g_label .3
g_pop 1
g_push 1
g_push 3
g_cons 1, 2
g_push 1
g_updcons 0, 2, 8
g_pop 7
g_return
g_jump .5
g_label .4
g_pop 1
g_push 1
g_push 1
g_push 4
g_cons 1, 2
g_updcons 0, 2, 8
g_pop 7
g_return
g_jump .5
g_label .5
g_jump .2
g_label .2

g_globstart partition, 2
g_alloc 1
g_push 0
g_push 2
g_pushglobal partition$ll1, 3
g_updap 2, 1
g_push 2
g_push 1
g_updap 1, 4
g_pop 3
g_unwind

g_globstart append, 2
g_push 0
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 2
g_update 1
g_unwind
g_label .1
g_uncons 2
g_push 3
g_push 2
g_pushglobal append, 2
g_mkap 2
g_push 1
g_updcons 1, 2, 5
g_pop 4
g_return
g_jump .2
g_label .2

g_globstart replicate, 2
g_pushint 0
g_push 1
g_eval
g_leq
g_jumpcase .0, .1
g_label .0
g_pop 1
g_push 1
g_pushint 1
g_push 2
g_pushglobal gm$sub, 2
g_mkap 2
g_pushglobal replicate, 2
g_mkap 2
g_push 2
g_updcons 1, 2, 3
g_pop 2
g_return
g_jump .2
g_label .1
g_pop 1
g_pushglobal gm$cons_0_0, 0
g_update 3
g_pop 2
g_unwind
g_label .2

g_globstart gm$return, 2
g_updcons 0, 2, 1
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

g_globstart op$s$ll1, 2
g_update 2
g_pop 1
g_unwind

g_globstart op$s, 2
g_push 1
g_pushglobal op$s$ll1, 2
g_mkap 1
g_push 1
g_pushglobal gm$bind, 3
g_updap 2, 3
g_pop 2
g_unwind

g_globstart sequence_io$ll2, 2
g_push 1
g_push 1
g_cons 1, 2
g_pushglobal gm$return, 2
g_updap 1, 3
g_pop 2
g_unwind

g_globstart sequence_io$ll1, 2
g_push 1
g_pushglobal sequence_io$ll2, 2
g_mkap 1
g_push 1
g_pushglobal sequence_io, 1
g_mkap 1
g_pushglobal gm$bind, 3
g_updap 2, 3
g_pop 2
g_unwind

g_globstart sequence_io, 1
g_push 0
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal gm$cons_0_0, 0
g_pushglobal gm$return, 2
g_updap 1, 2
g_pop 1
g_unwind
g_label .1
g_uncons 2
g_push 1
g_pushglobal sequence_io$ll1, 2
g_mkap 1
g_push 1
g_pushglobal gm$bind, 3
g_updap 2, 4
g_pop 3
g_unwind
g_label .2

g_globstart iter_io$ll1, 3
g_push 2
g_push 2
g_push 2
g_mkap 1
g_pushglobal op$s, 2
g_updap 2, 4
g_pop 3
g_unwind

g_globstart iter_io, 1
g_pushglobal gm$cons_0_0, 0
g_pushglobal gm$return, 2
g_mkap 1
g_push 1
g_pushglobal iter_io$ll1, 3
g_mkap 1
g_pushglobal foldr, 3
g_updap 2, 2
g_pop 1
g_unwind

g_globstart qsort$ll1, 2
g_push 0
g_eval
g_push 2
g_eval
g_les
g_update 3
g_pop 2
g_return

g_globstart qsort, 1
g_push 0
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal gm$cons_0_0, 0
g_update 2
g_pop 1
g_unwind
g_label .1
g_uncons 2
g_push 1
g_push 1
g_pushglobal qsort$ll1, 2
g_mkap 1
g_pushglobal partition, 2
g_mkap 2
g_eval
g_uncons 2
g_push 1
g_pushglobal qsort, 1
g_mkap 1
g_push 3
g_cons 1, 2
g_push 1
g_pushglobal qsort, 1
g_mkap 1
g_pushglobal append, 2
g_updap 2, 6
g_pop 5
g_unwind
g_label .2

g_globstart main$ll2, 1
g_push 0
g_pushglobal qsort, 1
g_mkap 1
g_pushglobal gm$print, 2
g_pushglobal iter_io, 1
g_updap 2, 2
g_pop 1
g_unwind

g_globstart main$ll1, 1
g_pushglobal main$ll2, 1
g_pushglobal gm$input, 1
g_push 2
g_pushglobal replicate, 2
g_mkap 2
g_pushglobal sequence_io, 1
g_mkap 1
g_pushglobal gm$bind, 3
g_updap 2, 2
g_pop 1
g_unwind

g_globstart main, 0
g_pushglobal main$ll1, 1
g_pushglobal gm$input, 1
g_pushglobal gm$bind, 3
g_updap 2, 1
g_unwind
