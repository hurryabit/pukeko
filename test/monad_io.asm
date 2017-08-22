g_declare_cafs gm$cons_0_0, main
g_declare_main main

g_globstart gm$cons_0_0, 0
g_updcons 0, 0, 1
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

g_globstart when, 2
g_push 0
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal gm$cons_0_0, 0
g_pushglobal gm$return, 2
g_updap 1, 3
g_pop 2
g_unwind
g_label .1
g_pop 2
g_update 1
g_unwind
g_label .2

g_globstart count_down, 1
g_pushint 1
g_push 1
g_pushglobal gm$sub, 2
g_mkap 2
g_pushglobal count_down, 1
g_mkap 1
g_push 1
g_pushglobal gm$print, 2
g_mkap 1
g_pushglobal op$s, 2
g_mkap 2
g_pushint 0
g_push 2
g_pushglobal gm$ge, 2
g_mkap 2
g_pushglobal when, 2
g_updap 2, 2
g_pop 1
g_unwind

g_globstart repeat_m, 2
g_push 1
g_pushint 1
g_push 2
g_pushglobal gm$sub, 2
g_mkap 2
g_pushglobal repeat_m, 2
g_mkap 2
g_push 2
g_pushglobal op$s, 2
g_mkap 2
g_pushint 0
g_push 2
g_pushglobal gm$gt, 2
g_mkap 2
g_pushglobal when, 2
g_updap 2, 3
g_pop 2
g_unwind

g_globstart main$ll2, 2
g_push 1
g_pushglobal count_down, 1
g_mkap 1
g_push 1
g_pushglobal repeat_m, 2
g_updap 2, 3
g_pop 2
g_unwind

g_globstart main$ll1, 1
g_push 0
g_pushglobal main$ll2, 2
g_mkap 1
g_pushglobal gm$input, 1
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
