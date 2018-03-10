g_declare_globals gm$cons_0_2, 2, gm$cons_0_4, 4, gm$neg, 1, gm$add, 2, gm$sub, 2, gm$mul, 2, gm$seq, 2, gm$puti, 1, dict$Ring$Int, 0, main, 0, io$ll1, 3, io$ll2, 2, print$ll1, 0
g_declare_main main

g_globstart gm$cons_0_2, 2
g_updcons 0, 2, 1
g_return

g_globstart gm$cons_0_4, 4
g_updcons 0, 4, 1
g_return

g_globstart gm$neg, 1
g_eval
g_neg
g_update 1
g_return

g_globstart gm$add, 2
g_push 1
g_eval
g_push 1
g_eval
g_add
g_update 3
g_pop 2
g_return

g_globstart gm$sub, 2
g_push 1
g_eval
g_push 1
g_eval
g_sub
g_update 3
g_pop 2
g_return

g_globstart gm$mul, 2
g_push 1
g_eval
g_push 1
g_eval
g_mul
g_update 3
g_pop 2
g_return

g_globstart gm$seq, 2
g_eval
g_pop 1
g_update 1
g_unwind

g_globstart gm$puti, 1
g_eval
g_print
g_updcons 0, 0, 1
g_return

g_globstart dict$Ring$Int, 0
g_pushglobal gm$mul
g_pushglobal gm$sub
g_pushglobal gm$add
g_pushglobal gm$neg
g_updcons 0, 4, 1
g_return

g_globstart main, 0
g_pushint 2
g_pushint 2
g_pushglobal dict$Ring$Int
g_eval
g_proj 1
g_push 0
g_slide 1
g_mkap 2
g_pushint 1
g_slide 1
g_pushglobal print$ll1
g_updap 1, 1
g_unwind

g_globstart io$ll1, 3
g_push 1
g_push 1
g_mkap 1
g_push 3
g_push 1
g_cons 0, 2
g_push 1
g_pushglobal gm$seq
g_updap 2, 5
g_pop 4
g_unwind

g_globstart io$ll2, 2
g_push 1
g_push 1
g_pushglobal io$ll1
g_updap 2, 3
g_pop 2
g_unwind

g_globstart print$ll1, 0
g_pushglobal gm$puti
g_pushglobal io$ll2
g_updap 1, 1
g_unwind
