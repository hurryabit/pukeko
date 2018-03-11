g_declare_globals gm$cons_0_0, 0, gm$cons_0_2, 2, main, 0
g_declare_main main

g_globstart gm$cons_0_0, 0
g_updcons 0, 0, 1
g_return

g_globstart gm$cons_0_2, 2
g_updcons 0, 2, 1
g_return

g_globstart main, 0
g_pushglobal gm$cons_0_0
g_pushglobal gm$cons_0_2
g_updap 1, 1
g_unwind
