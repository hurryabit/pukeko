g_declare_cafs gm$cons_0_0, dict$Monad$IO, dict$Monad$IO$ll1, input, main
g_declare_main main

g_globstart gm$cons_0_0, 0
g_updcons 0, 0, 1
g_return

g_globstart gm$cons_0_2, 2
g_updcons 0, 2, 1
g_return

g_globstart gm$geti, 1
g_pop 1
g_input
g_update 1
g_return

g_globstart gm$puti, 1
g_eval
g_print
g_updcons 0, 0, 1
g_return

g_globstart gm$seq, 2
g_eval
g_pop 1
g_update 1
g_unwind

g_globstart bind$ll1, 1
g_push 0
g_eval
g_proj 1
g_update 2
g_pop 1
g_unwind

g_globstart dict$Monad$IO, 0
g_pushglobal dict$Monad$IO$ll2, 1
g_pushglobal dict$Monad$IO$ll4, 2
g_push 0
g_push 2
g_updcons 0, 2, 3
g_pop 2
g_return

g_globstart dict$Monad$IO$ll1, 0
g_pushglobal gm$cons_0_2, 2
g_update 1
g_unwind

g_globstart dict$Monad$IO$ll2, 1
g_push 0
g_pushglobal dict$Monad$IO$ll1, 0
g_updap 1, 2
g_pop 1
g_unwind

g_globstart dict$Monad$IO$ll3, 3
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

g_globstart dict$Monad$IO$ll4, 2
g_push 1
g_push 1
g_pushglobal dict$Monad$IO$ll3, 3
g_updap 2, 3
g_pop 2
g_unwind

g_globstart fst$ll1, 1
g_push 0
g_eval
g_proj 0
g_update 2
g_pop 1
g_unwind

g_globstart input, 0
g_pushglobal gm$cons_0_0, 0
g_pushglobal gm$geti, 1
g_pushglobal io$ll2, 2
g_updap 2, 1
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

g_globstart main, 0
g_pushglobal main$ll2, 1
g_pushglobal input, 0
g_pushglobal dict$Monad$IO, 0
g_pushglobal bind$ll1, 1
g_updap 3, 1
g_unwind

g_globstart main$ll1, 2
g_push 1
g_push 1
g_cons 0, 2
g_push 0
g_pushglobal snd$ll1, 1
g_mkap 1
g_pushglobal print$ll1, 1
g_mkap 1
g_push 1
g_pushglobal fst$ll1, 1
g_mkap 1
g_pushglobal print$ll1, 1
g_mkap 1
g_pushglobal dict$Monad$IO, 0
g_pushglobal semi$ll2, 3
g_updap 3, 4
g_pop 3
g_unwind

g_globstart main$ll2, 1
g_push 0
g_pushglobal main$ll1, 2
g_mkap 1
g_pushglobal input, 0
g_pushglobal dict$Monad$IO, 0
g_pushglobal bind$ll1, 1
g_updap 3, 2
g_pop 1
g_unwind

g_globstart print$ll1, 1
g_push 0
g_pushglobal gm$puti, 1
g_pushglobal io$ll2, 2
g_updap 2, 2
g_pop 1
g_unwind

g_globstart semi$ll1, 2
g_update 2
g_pop 1
g_unwind

g_globstart semi$ll2, 3
g_push 2
g_pushglobal semi$ll1, 2
g_mkap 1
g_push 2
g_push 2
g_pushglobal bind$ll1, 1
g_updap 3, 4
g_pop 3
g_unwind

g_globstart snd$ll1, 1
g_push 0
g_eval
g_proj 1
g_update 2
g_pop 1
g_unwind
