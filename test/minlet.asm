g_declare_cafs main
g_declare_main main

g_globstart print, 2
g_eval
g_print
g_cons 0, 0
g_cons 0, 2
g_update 1
g_return

g_globstart id, 1
g_push 0
g_push 0
g_eval
g_update 3
g_pop 2
g_unwind

g_globstart main, 0
g_pushint 0
g_pushglobal id, 1
g_mkap 1
g_pushglobal print, 2
g_mkap 1
g_update 1
g_pop 0
g_unwind
