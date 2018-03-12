g_declare_globals C.0.2, 2, C.0.4, 4, B.neg, 1, B.add, 2, B.sub, 2, B.mul, 2, B.seq, 2, B.puti, 1, ringInt, 0, main, 0, add.L1, 1, io.L1, 3, io.L2, 2, print.L1, 0, g.L1, 4, h.L1, 1
g_declare_main main

g_globstart C.0.2, 2
g_updcons 0, 2, 1
g_return

g_globstart C.0.4, 4
g_updcons 0, 4, 1
g_return

g_globstart B.neg, 1
g_eval
g_neg
g_update 1
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

g_globstart B.sub, 2
g_push 1
g_eval
g_push 1
g_eval
g_sub
g_update 3
g_pop 2
g_return

g_globstart B.mul, 2
g_push 1
g_eval
g_push 1
g_eval
g_mul
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

g_globstart ringInt, 0
g_pushglobal B.mul
g_pushglobal B.sub
g_pushglobal B.add
g_pushglobal B.neg
g_updcons 0, 4, 1
g_return

g_globstart main, 0
g_pushint 2
g_pushglobal h.L1
g_mkap 1
g_pushint 3
g_pushint 2
g_pushint 1
g_pushglobal g.L1
g_mkap 4
g_pushglobal print.L1
g_updap 1, 1
g_unwind

g_globstart add.L1, 1
g_push 0
g_eval
g_proj 1
g_update 2
g_pop 1
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

g_globstart print.L1, 0
g_pushglobal B.puti
g_pushglobal io.L2
g_updap 1, 1
g_unwind

g_globstart g.L1, 4
g_update 4
g_pop 3
g_unwind

g_globstart h.L1, 1
g_push 0
g_push 1
g_pushglobal ringInt
g_pushglobal add.L1
g_updap 3, 2
g_pop 1
g_unwind
