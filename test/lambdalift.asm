g_declare_cafs dict$Ring$Int, main, print$ll1
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
g_pushglobal gm$neg, 1
g_pushglobal gm$add, 2
g_pushglobal gm$sub, 2
g_pushglobal gm$mul, 2
g_push 0
g_push 2
g_push 4
g_push 6
g_updcons 0, 4, 5
g_pop 4
g_return

g_globstart main, 0
g_pushint 2
g_pushglobal h$ll1, 1
g_mkap 1
g_pushint 3
g_pushint 2
g_pushint 1
g_pushglobal g$ll1, 4
g_mkap 4
g_pushglobal print$ll1, 0
g_updap 1, 1
g_unwind

g_globstart add$ll1, 1
g_push 0
g_eval
g_proj 1
g_update 2
g_pop 1
g_unwind

g_globstart io$ll1, 3
g_push 1
g_push 1
g_mkap 1
g_push 3
g_push 1
g_cons 0, 2
g_push 1
g_pushglobal gm$seq, 2
g_updap 2, 5
g_pop 4
g_unwind

g_globstart io$ll2, 2
g_push 1
g_push 1
g_pushglobal io$ll1, 3
g_updap 2, 3
g_pop 2
g_unwind

g_globstart print$ll1, 0
g_pushglobal gm$puti, 1
g_pushglobal io$ll2, 2
g_updap 1, 1
g_unwind

g_globstart g$ll1, 4
g_update 4
g_pop 3
g_unwind

g_globstart h$ll1, 1
g_push 0
g_push 1
g_pushglobal dict$Ring$Int, 0
g_pushglobal add$ll1, 1
g_updap 3, 2
g_pop 1
g_unwind
