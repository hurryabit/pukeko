g_declare_cafs gm$cons_0_0, dict$Monad$IO, random, main, print$ll1
g_declare_main main

g_globstart gm$cons_0_0, 0
g_updcons 0, 0, 1
g_return

g_globstart gm$cons_0_2, 2
g_updcons 0, 2, 1
g_return

g_globstart gm$cons_1_2, 2
g_updcons 1, 2, 1
g_return

g_globstart gm$lt, 2
g_push 1
g_eval
g_push 1
g_eval
g_les
g_update 3
g_pop 2
g_return

g_globstart gm$le, 2
g_push 1
g_eval
g_push 1
g_eval
g_leq
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

g_globstart gm$mod, 2
g_push 1
g_eval
g_push 1
g_eval
g_mod
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

g_globstart dict$Monad$IO, 0
g_pushglobal dict$Monad$IO$ll4, 2
g_pushglobal dict$Monad$IO$ll2, 1
g_updcons 0, 2, 1
g_return

g_globstart random, 0
g_pushint 1
g_pushglobal random$ll1, 1
g_pushglobal gen$ll1, 2
g_updap 2, 1
g_unwind

g_globstart main, 0
g_pushint 400000
g_pushglobal print$ll1, 0
g_mkap 1
g_pushint 100000
g_pushglobal print$ll1, 0
g_mkap 1
g_pushglobal random, 0
g_pushint 400000
g_pushglobal split_at$ll1, 2
g_mkap 2
g_eval
g_uncons 2
g_push 0
g_pushglobal gm$cons_0_0, 0
g_pushglobal gm$cons_0_2, 2
g_mkap 1
g_pushglobal print$ll1, 0
g_pushglobal dict$Monad$IO, 0
g_pushglobal traverse_$ll1, 3
g_mkap 2
g_pushglobal dict$Foldable$List$ll1, 3
g_mkap 3
g_push 2
g_pushint 100000
g_pushglobal split_at$ll1, 2
g_mkap 2
g_eval
g_uncons 2
g_push 1
g_pushint 100000
g_pushglobal take$ll1, 2
g_mkap 2
g_push 0
g_push 2
g_pushint 400000
g_pushglobal main$ll1, 3
g_mkap 1
g_pushglobal zip_with$ll1, 3
g_mkap 3
g_pushglobal dict$Monad$IO, 0
g_pushglobal sequence$ll3, 2
g_mkap 2
g_pushglobal main$ll2, 1
g_push 1
g_pushglobal dict$Monad$IO$ll3, 3
g_mkap 2
g_slide 1
g_slide 1
g_slide 2
g_push 0
g_pushglobal semi$ll1, 2
g_mkap 1
g_push 0
g_push 3
g_pushglobal dict$Monad$IO$ll3, 3
g_mkap 2
g_slide 1
g_slide 2
g_slide 2
g_push 0
g_pushglobal semi$ll1, 2
g_mkap 1
g_push 0
g_push 3
g_pushglobal dict$Monad$IO$ll3, 3
g_mkap 2
g_slide 1
g_slide 2
g_push 0
g_pushglobal semi$ll1, 2
g_mkap 1
g_push 0
g_push 3
g_pushglobal dict$Monad$IO$ll3, 3
g_updap 2, 4
g_pop 3
g_unwind

g_globstart dict$Foldable$List$ll1, 3
g_push 2
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 2
g_update 2
g_pop 1
g_unwind
g_label .1
g_uncons 2
g_push 1
g_push 4
g_push 4
g_pushglobal dict$Foldable$List$ll1, 3
g_mkap 3
g_push 1
g_push 4
g_updap 2, 6
g_pop 5
g_unwind
g_label .2

g_globstart take$ll1, 2
g_pushint 0
g_push 1
g_eval
g_leq
g_jumpcase .0, .1
g_label .0
g_pop 1
g_push 1
g_eval
g_jumpcase .3, .4
g_label .3
g_pop 1
g_pushglobal gm$cons_0_0, 0
g_update 3
g_pop 2
g_unwind
g_label .4
g_uncons 2
g_push 1
g_pushint 1
g_push 4
g_pushglobal gm$sub, 2
g_mkap 2
g_pushglobal take$ll1, 2
g_mkap 2
g_push 1
g_updcons 1, 2, 5
g_pop 4
g_return
g_jump .5
g_label .5
g_jump .2
g_label .1
g_pop 1
g_pushglobal gm$cons_0_0, 0
g_update 3
g_pop 2
g_unwind
g_label .2

g_globstart zip_with$ll1, 3
g_push 1
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal gm$cons_0_0, 0
g_update 4
g_pop 3
g_unwind
g_label .1
g_uncons 2
g_push 4
g_eval
g_jumpcase .3, .4
g_label .3
g_pop 1
g_pushglobal gm$cons_0_0, 0
g_update 6
g_pop 5
g_unwind
g_label .4
g_uncons 2
g_push 1
g_push 4
g_push 6
g_pushglobal zip_with$ll1, 3
g_mkap 3
g_push 1
g_push 4
g_push 7
g_mkap 2
g_updcons 1, 2, 8
g_pop 7
g_return
g_jump .5
g_label .5
g_jump .2
g_label .2

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
g_eval
g_proj 1
g_push 0
g_slide 1
g_updap 2, 4
g_pop 3
g_unwind

g_globstart sequence$ll1, 3
g_push 2
g_push 2
g_cons 1, 2
g_push 1
g_eval
g_proj 0
g_push 0
g_slide 1
g_updap 1, 4
g_pop 3
g_unwind

g_globstart sequence$ll2, 3
g_push 2
g_push 1
g_pushglobal sequence$ll1, 3
g_mkap 2
g_push 2
g_push 2
g_pushglobal sequence$ll3, 2
g_mkap 2
g_push 2
g_eval
g_proj 1
g_push 0
g_slide 1
g_updap 2, 4
g_pop 3
g_unwind

g_globstart sequence$ll3, 2
g_push 1
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal gm$cons_0_0, 0
g_push 1
g_eval
g_proj 0
g_push 0
g_slide 1
g_updap 1, 3
g_pop 2
g_unwind
g_label .1
g_uncons 2
g_push 1
g_push 3
g_pushglobal sequence$ll2, 3
g_mkap 2
g_push 1
g_push 4
g_eval
g_proj 1
g_push 0
g_slide 1
g_updap 2, 5
g_pop 4
g_unwind
g_label .2

g_globstart traverse_$ll1, 3
g_push 2
g_push 2
g_mkap 1
g_push 1
g_pushglobal semi$ll2, 3
g_updap 2, 4
g_pop 3
g_unwind

g_globstart dict$Monad$IO$ll2, 1
g_push 0
g_pushglobal gm$cons_0_2, 2
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

g_globstart gen$ll1, 2
g_push 1
g_push 1
g_mkap 1
g_push 1
g_pushglobal gen$ll1, 2
g_mkap 2
g_push 2
g_updcons 1, 2, 3
g_pop 2
g_return

g_globstart split_at$ll1, 2
g_pushint 0
g_push 1
g_eval
g_leq
g_jumpcase .0, .1
g_label .0
g_pop 1
g_push 1
g_eval
g_jumpcase .3, .4
g_label .3
g_pop 1
g_pushglobal gm$cons_0_0, 0
g_pushglobal gm$cons_0_0, 0
g_updcons 0, 2, 3
g_pop 2
g_return
g_jump .5
g_label .4
g_uncons 2
g_push 1
g_pushint 1
g_push 4
g_pushglobal gm$sub, 2
g_mkap 2
g_pushglobal split_at$ll1, 2
g_mkap 2
g_eval
g_uncons 2
g_push 1
g_push 1
g_push 4
g_cons 1, 2
g_updcons 0, 2, 7
g_pop 6
g_return
g_jump .5
g_label .5
g_jump .2
g_label .1
g_pop 1
g_push 1
g_pushglobal gm$cons_0_0, 0
g_updcons 0, 2, 3
g_pop 2
g_return
g_jump .2
g_label .2

g_globstart random$ll1, 1
g_pushint 1000000007
g_push 1
g_eval
g_pushint 91
g_mul
g_mod
g_update 2
g_pop 1
g_return

g_globstart main$ll1, 3
g_push 0
g_push 2
g_pushglobal gm$mod, 2
g_mkap 2
g_push 1
g_push 4
g_pushglobal gm$mod, 2
g_mkap 2
g_push 0
g_eval
g_push 2
g_eval
g_les
g_jumpcase .0, .1
g_label .0
g_pop 1
g_push 0
g_pushglobal print$ll1, 0
g_mkap 1
g_push 2
g_pushglobal print$ll1, 0
g_mkap 1
g_push 0
g_pushglobal semi$ll1, 2
g_mkap 1
g_push 0
g_push 3
g_pushglobal dict$Monad$IO$ll3, 3
g_updap 2, 9
g_pop 8
g_unwind
g_label .1
g_pop 1
g_push 1
g_pushglobal print$ll1, 0
g_mkap 1
g_push 1
g_pushglobal print$ll1, 0
g_mkap 1
g_push 0
g_pushglobal semi$ll1, 2
g_mkap 1
g_push 0
g_push 3
g_pushglobal dict$Monad$IO$ll3, 3
g_updap 2, 9
g_pop 8
g_unwind
g_label .2

g_globstart main$ll2, 1
g_pushglobal gm$cons_0_0, 0
g_pushglobal gm$cons_0_2, 2
g_updap 1, 2
g_pop 1
g_unwind
