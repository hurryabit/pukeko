g_declare_cafs main
g_declare_main main

g_globstart gm$print, 2
g_eval
g_print
g_cons 0, 0
g_updcons 0, 2, 1
g_return

g_globstart g, 4
g_update 4
g_pop 3
g_unwind

g_globstart main, 0
g_pushint 4
g_pushint 3
g_pushint 2
g_pushint 1
g_pushglobal g, 4
g_mkap 4
g_pushglobal gm$print, 2
g_updap 1, 1
g_unwind