g_declare_cafs gm$cons_0_0, gm$abort, psums, primes, main
g_declare_main main

g_globstart gm$cons_0_0, 0
g_updcons 0, 0, 1
g_return

g_globstart gm$cons_1_2, 2
g_updcons 1, 2, 1
g_return

g_globstart gm$abort, 0
g_abort

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

g_globstart gm$mod, 2
g_push 1
g_eval
g_push 1
g_eval
g_mod
g_update 3
g_pop 2
g_return

g_globstart gm$ne, 2
g_push 1
g_eval
g_push 1
g_eval
g_neq
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

g_globstart nth, 2
g_push 0
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal gm$abort, 0
g_update 3
g_pop 2
g_unwind
g_label .1
g_uncons 2
g_pushint 0
g_push 4
g_eval
g_leq
g_jumpcase .3, .4
g_label .3
g_pop 1
g_pushint 1
g_push 4
g_pushglobal gm$sub, 2
g_mkap 2
g_push 2
g_pushglobal nth, 2
g_updap 2, 5
g_pop 4
g_unwind
g_label .4
g_pop 1
g_update 4
g_pop 3
g_unwind
g_label .5
g_jump .2
g_label .2

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

g_globstart repeat, 1
g_alloc 1
g_push 0
g_push 2
g_pushglobal append, 2
g_updap 2, 1
g_update 2
g_pop 1
g_unwind

g_globstart psums$ll1, 3
g_push 2
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal gm$cons_0_0, 0
g_update 4
g_pop 3
g_unwind
g_label .1
g_uncons 2
g_push 3
g_push 1
g_pushglobal gm$add, 2
g_mkap 2
g_push 2
g_push 1
g_push 5
g_mkap 2
g_push 1
g_updcons 1, 2, 7
g_pop 6
g_return
g_jump .2
g_label .2

g_globstart psums, 0
g_alloc 1
g_push 0
g_pushglobal psums$ll1, 3
g_updap 1, 1
g_pushint 0
g_push 1
g_updap 1, 2
g_pop 1
g_unwind

g_globstart filter$ll1, 3
g_push 2
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal gm$cons_0_0, 0
g_update 4
g_pop 3
g_unwind
g_label .1
g_uncons 2
g_push 1
g_push 4
g_mkap 1
g_push 1
g_push 4
g_mkap 1
g_eval
g_jumpcase .3, .4
g_label .3
g_pop 1
g_update 6
g_pop 5
g_unwind
g_label .4
g_pop 1
g_push 0
g_push 2
g_updcons 1, 2, 7
g_pop 6
g_return
g_jump .5
g_label .5
g_jump .2
g_label .2

g_globstart filter, 1
g_alloc 1
g_push 0
g_push 2
g_pushglobal filter$ll1, 3
g_updap 2, 1
g_update 2
g_pop 1
g_unwind

g_globstart sieve$ll1, 2
g_pushint 0
g_push 1
g_eval
g_push 3
g_eval
g_mod
g_neq
g_update 3
g_pop 2
g_return

g_globstart sieve, 1
g_push 0
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal gm$abort, 0
g_update 2
g_pop 1
g_unwind
g_label .1
g_uncons 2
g_push 1
g_push 1
g_pushglobal sieve$ll1, 2
g_mkap 1
g_pushglobal filter, 1
g_mkap 2
g_pushglobal sieve, 1
g_mkap 1
g_push 1
g_updcons 1, 2, 4
g_pop 3
g_return
g_jump .2
g_label .2

g_globstart primes, 0
g_pushglobal gm$cons_0_0, 0
g_pushint 4
g_cons 1, 2
g_pushint 2
g_cons 1, 2
g_pushglobal repeat, 1
g_mkap 1
g_pushint 5
g_cons 1, 2
g_pushglobal psums, 0
g_mkap 1
g_pushglobal sieve, 1
g_mkap 1
g_pushint 3
g_cons 1, 2
g_pushint 2
g_updcons 1, 2, 1
g_return

g_globstart main$ll1, 1
g_push 0
g_pushglobal primes, 0
g_pushglobal nth, 2
g_mkap 2
g_pushglobal gm$print, 2
g_updap 1, 2
g_pop 1
g_unwind

g_globstart main, 0
g_pushglobal main$ll1, 1
g_pushglobal gm$input, 1
g_pushglobal gm$bind, 3
g_updap 2, 1
g_unwind
