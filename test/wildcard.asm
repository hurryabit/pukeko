g_declare_cafs main
g_declare_main main

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
g_update 2
g_pop 1
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

g_globstart fst, 1
g_push 0
g_eval
g_uncons 2
g_update 3
g_pop 2
g_unwind

g_globstart snd, 1
g_push 0
g_eval
g_uncons 2
g_pop 1
g_update 2
g_pop 1
g_unwind

g_globstart main$2, 2
g_push 1
g_push 1
g_cons 0, 2
g_push 0
g_pushglobal snd, 1
g_mkap 1
g_pushglobal print, 2
g_mkap 1
g_push 1
g_pushglobal fst, 1
g_mkap 1
g_pushglobal print, 2
g_mkap 1
g_pushglobal prefix_semi, 2
g_updap 2, 4
g_pop 3
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
