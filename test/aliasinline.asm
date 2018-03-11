g_declare_globals C.0.0, 0, C.0.2, 2, main, 0
g_declare_main main

g_globstart C.0.0, 0
g_updcons 0, 0, 1
g_return

g_globstart C.0.2, 2
g_updcons 0, 2, 1
g_return

g_globstart main, 0
g_pushglobal C.0.0
g_pushglobal C.0.2
g_updap 1, 1
g_unwind
