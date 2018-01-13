g_declare_cafs gm$cons_0_0, main
g_declare_main main

g_globstart gm$cons_0_0, 0
g_updcons 0, 0, 1
g_return

g_globstart gm$return, 2
g_updcons 0, 2, 1
g_return

g_globstart h, 1
g_update 1
g_unwind

g_globstart main, 0
g_pushglobal gm$cons_0_0, 0
g_pushglobal h, 1
g_mkap 1
g_pushglobal h, 1
g_mkap 1
g_pushglobal gm$return, 2
g_updap 1, 1
g_unwind
