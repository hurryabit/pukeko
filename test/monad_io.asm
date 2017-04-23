g_declare_cafs Unit, main
g_declare_main main

g_globstart prefix_sub, 2
g_push 1
g_eval
g_push 1
g_eval
g_sub
g_update 3
g_pop 2
g_return

g_globstart prefix_ge, 2
g_push 1
g_eval
g_push 1
g_eval
g_geq
g_update 3
g_pop 2
g_return

g_globstart prefix_gt, 2
g_push 1
g_eval
g_push 1
g_eval
g_gtr
g_update 3
g_pop 2
g_return

g_globstart Unit, 0
g_updcons 0, 0, 1
g_return

g_globstart return, 2
g_updcons 0, 2, 1
g_return

g_globstart print, 2
g_eval
g_print
g_cons 0, 0
g_updcons 0, 2, 1
g_return

g_globstart input, 1
g_input
g_updcons 0, 2, 1
g_return

g_globstart prefix_bind, 3
g_push 2
g_push 1
g_mkap 1
g_eval
g_uncons 2
g_push 3
g_updap 2, 4
g_pop 3
g_unwind

g_globstart prefix_semi$1, 2
g_push 0
g_update 3
g_pop 2
g_unwind

g_globstart prefix_semi, 2
g_push 1
g_pushglobal prefix_semi$1, 2
g_mkap 1
g_push 1
g_pushglobal prefix_bind, 3
g_updap 2, 3
g_pop 2
g_unwind

g_globstart when, 2
g_push 0
g_eval
g_jumpcase .0, .1
g_label .0
g_uncons 0
g_pushglobal Unit, 0
g_pushglobal return, 2
g_updap 1, 3
g_pop 2
g_unwind
g_label .1
g_uncons 0
g_push 1
g_update 3
g_pop 2
g_unwind
g_label .2

g_globstart count_down, 1
g_pushint 1
g_push 1
g_pushglobal prefix_sub, 2
g_mkap 2
g_pushglobal count_down, 1
g_mkap 1
g_push 1
g_pushglobal print, 2
g_mkap 1
g_pushglobal prefix_semi, 2
g_mkap 2
g_pushint 0
g_push 2
g_pushglobal prefix_ge, 2
g_mkap 2
g_pushglobal when, 2
g_updap 2, 2
g_pop 1
g_unwind

g_globstart repeat_m, 2
g_push 1
g_pushint 1
g_push 2
g_pushglobal prefix_sub, 2
g_mkap 2
g_pushglobal repeat_m, 2
g_mkap 2
g_push 2
g_pushglobal prefix_semi, 2
g_mkap 2
g_pushint 0
g_push 2
g_pushglobal prefix_gt, 2
g_mkap 2
g_pushglobal when, 2
g_updap 2, 3
g_pop 2
g_unwind

g_globstart main$2, 2
g_push 1
g_pushglobal count_down, 1
g_mkap 1
g_push 1
g_pushglobal repeat_m, 2
g_updap 2, 3
g_pop 2
g_unwind

g_globstart main$1, 1
g_push 0
g_pushglobal main$2, 2
g_mkap 1
g_pushglobal input, 1
g_pushglobal prefix_bind, 3
g_updap 2, 2
g_pop 1
g_unwind

g_globstart main, 0
g_pushglobal main$1, 1
g_pushglobal input, 1
g_pushglobal prefix_bind, 3
g_updap 2, 1
g_unwind
