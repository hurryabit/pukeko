g_declare_globals C.0.2, 2, B.add, 2, B.seq, 2, B.puti, 1, print, 0, main, 0, io.L1, 3, io.L2, 2
g_declare_main main

g_globstart C.0.2, 2
g_updcons 0, 2, 1
g_return

g_globstart B.add, 2
g_push 1
g_eval
g_push 1
g_eval
g_add
g_update 3
g_pop 2
g_return

g_globstart B.seq, 2
g_eval
g_pop 1
g_update 1
g_unwind

g_globstart B.puti, 1
g_eval
g_print
g_updcons 0, 0, 1
g_return

g_globstart print, 0
g_pushglobal B.puti
g_pushglobal io.L2
g_updap 1, 1
g_unwind

g_globstart main, 0
g_pushint 2
g_pushint 2
g_pushglobal B.add
g_mkap 2
g_pushint 1
g_slide 1
g_pushglobal print
g_updap 1, 1
g_unwind

g_globstart io.L1, 3
g_push 1
g_push 1
g_mkap 1
g_push 3
g_push 1
g_cons 0, 2
g_push 1
g_pushglobal B.seq
g_updap 2, 5
g_pop 4
g_unwind

g_globstart io.L2, 2
g_push 1
g_push 1
g_pushglobal io.L1
g_updap 2, 3
g_pop 2
g_unwind
