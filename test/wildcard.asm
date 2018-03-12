g_declare_globals C.0.0, 0, C.0.2, 2, B.seq, 2, B.puti, 1, B.geti, 1, bind, 1, monadIO, 0, print, 0, input, 0, main, 0, semi.L1, 2, semi.L2, 3, monadIO.pure.L1, 0, monadIO.pure.L2, 1, monadIO.bind.L1, 3, monadIO.bind.L2, 2, io.L1, 3, io.L2, 2, fst.L1, 1, snd.L1, 1, main.L1, 2, main.L2, 1
g_declare_main main

g_globstart C.0.0, 0
g_updcons 0, 0, 1
g_return

g_globstart C.0.2, 2
g_updcons 0, 2, 1
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

g_globstart B.geti, 1
g_pop 1
g_input
g_update 1
g_return

g_globstart bind, 1
g_push 0
g_eval
g_proj 1
g_update 2
g_pop 1
g_unwind

g_globstart monadIO, 0
g_pushglobal monadIO.bind.L2
g_pushglobal monadIO.pure.L2
g_updcons 0, 2, 1
g_return

g_globstart print, 0
g_pushglobal B.puti
g_pushglobal io.L2
g_updap 1, 1
g_unwind

g_globstart input, 0
g_pushglobal C.0.0
g_pushglobal B.geti
g_pushglobal io.L2
g_updap 2, 1
g_unwind

g_globstart main, 0
g_pushglobal main.L2
g_pushglobal input
g_pushglobal monadIO
g_pushglobal bind
g_updap 3, 1
g_unwind

g_globstart semi.L1, 2
g_update 2
g_pop 1
g_unwind

g_globstart semi.L2, 3
g_push 2
g_pushglobal semi.L1
g_mkap 1
g_push 2
g_push 2
g_pushglobal bind
g_updap 3, 4
g_pop 3
g_unwind

g_globstart monadIO.pure.L1, 0
g_pushglobal C.0.2
g_update 1
g_unwind

g_globstart monadIO.pure.L2, 1
g_push 0
g_pushglobal monadIO.pure.L1
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

g_globstart fst.L1, 1
g_push 0
g_eval
g_proj 0
g_update 2
g_pop 1
g_unwind

g_globstart snd.L1, 1
g_push 0
g_eval
g_proj 1
g_update 2
g_pop 1
g_unwind

g_globstart main.L1, 2
g_push 1
g_push 1
g_cons 0, 2
g_push 0
g_pushglobal snd.L1
g_mkap 1
g_pushglobal print
g_mkap 1
g_push 1
g_pushglobal fst.L1
g_mkap 1
g_pushglobal print
g_mkap 1
g_pushglobal monadIO
g_pushglobal semi.L2
g_updap 3, 4
g_pop 3
g_unwind

g_globstart main.L2, 1
g_push 0
g_pushglobal main.L1
g_mkap 1
g_pushglobal input
g_pushglobal monadIO
g_pushglobal bind
g_updap 3, 2
g_pop 1
g_unwind
