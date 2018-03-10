g_declare_globals C.0.0, 0, C.0.2, 2, monadIO, 0, main, 0, monadIO.pure.L2, 1, monadIO.bind.L1, 3, monadIO.bind.L2, 2
g_declare_main main

g_globstart C.0.0, 0
g_updcons 0, 0, 1
g_return

g_globstart C.0.2, 2
g_updcons 0, 2, 1
g_return

g_globstart monadIO, 0
g_pushglobal monadIO.bind.L2
g_pushglobal monadIO.pure.L2
g_updcons 0, 2, 1
g_return

g_globstart main, 0
g_pushglobal monadIO
g_pushglobal C.0.0
g_push 0
g_slide 1
g_push 0
g_slide 1
g_push 1
g_eval
g_proj 0
g_push 0
g_slide 1
g_updap 1, 2
g_pop 1
g_unwind

g_globstart monadIO.pure.L2, 1
g_push 0
g_pushglobal C.0.2
g_updap 1, 2
g_pop 1
g_unwind

g_globstart monadIO.bind.L1, 3
g_push 2
g_push 1
g_mkap 1
g_eval
g_uncons 2
g_push 1
g_push 1
g_push 5
g_updap 2, 6
g_pop 5
g_unwind

g_globstart monadIO.bind.L2, 2
g_push 1
g_push 1
g_pushglobal monadIO.bind.L1
g_updap 2, 3
g_pop 2
g_unwind
