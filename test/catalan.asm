g_declare_globals gm$cons_0_0, 0, gm$cons_0_1, 1, gm$cons_0_2, 2, gm$cons_0_4, 4, gm$cons_1_2, 2, gm$abort, 0, gm$lt, 2, gm$le, 2, gm$ge, 2, gm$gt, 2, gm$neg, 1, gm$add, 2, gm$sub, 2, gm$mul, 2, gm$mod, 2, gm$seq, 2, gm$puti, 1, gm$geti, 1, dict$Ord$Int, 0, dict$Ring$Int, 0, dict$Functor$List, 0, dict$Foldable$List, 0, dict$Monad$IO, 0, input, 0, p, 0, sum_p, 0, sols, 0, main, 0, dict$Functor$List$ll1, 2, dict$Foldable$List$ll1, 3, dict$Foldable$List$ll2, 3, nth_exn$ll1, 2, zip_with$ll1, 3, dict$Monad$IO$ll2, 1, dict$Monad$IO$ll3, 3, dict$Monad$IO$ll4, 2, io$ll1, 3, io$ll2, 2, print$ll1, 0, mul_p$ll1, 2, add_p$ll1, 2, scanl$ll1, 4, sols$ll1, 1, sols$ll2, 2, main$ll1, 1
g_declare_main main

g_globstart gm$cons_0_0, 0
g_updcons 0, 0, 1
g_return

g_globstart gm$cons_0_1, 1
g_updcons 0, 1, 1
g_return

g_globstart gm$cons_0_2, 2
g_updcons 0, 2, 1
g_return

g_globstart gm$cons_0_4, 4
g_updcons 0, 4, 1
g_return

g_globstart gm$cons_1_2, 2
g_updcons 1, 2, 1
g_return

g_globstart gm$abort, 0
g_abort

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

g_globstart gm$ge, 2
g_push 1
g_eval
g_push 1
g_eval
g_geq
g_update 3
g_pop 2
g_return

g_globstart gm$gt, 2
g_push 1
g_eval
g_push 1
g_eval
g_gtr
g_update 3
g_pop 2
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

g_globstart gm$geti, 1
g_pop 1
g_input
g_update 1
g_return

g_globstart dict$Ord$Int, 0
g_pushglobal gm$lt
g_pushglobal gm$le
g_pushglobal gm$gt
g_pushglobal gm$ge
g_updcons 0, 4, 1
g_return

g_globstart dict$Ring$Int, 0
g_pushglobal gm$mul
g_pushglobal gm$sub
g_pushglobal gm$add
g_pushglobal gm$neg
g_updcons 0, 4, 1
g_return

g_globstart dict$Functor$List, 0
g_pushglobal dict$Functor$List$ll1
g_updcons 0, 1, 1
g_return

g_globstart dict$Foldable$List, 0
g_pushglobal dict$Foldable$List$ll2
g_pushglobal dict$Foldable$List$ll1
g_updcons 0, 2, 1
g_return

g_globstart dict$Monad$IO, 0
g_pushglobal dict$Monad$IO$ll4
g_pushglobal dict$Monad$IO$ll2
g_updcons 0, 2, 1
g_return

g_globstart input, 0
g_pushglobal gm$cons_0_0
g_pushglobal gm$geti
g_pushglobal io$ll1
g_updap 2, 1
g_unwind

g_globstart p, 0
g_pushint 100000007
g_update 1
g_return

g_globstart sum_p, 0
g_pushint 0
g_pushglobal add_p$ll1
g_pushglobal dict$Foldable$List
g_eval
g_proj 1
g_push 0
g_slide 1
g_updap 2, 1
g_unwind

g_globstart sols, 0
g_alloc 1
g_push 0
g_pushglobal sols$ll2
g_pushglobal scanl$ll1
g_updap 2, 1
g_pushglobal sols
g_pushglobal gm$cons_0_0
g_push 2
g_mkap 2
g_slide 1
g_pushglobal sols$ll1
g_pushglobal dict$Functor$List
g_eval
g_proj 0
g_push 0
g_slide 1
g_mkap 2
g_pushint 1
g_updcons 1, 2, 1
g_return

g_globstart main, 0
g_pushglobal main$ll1
g_pushglobal input
g_pushglobal dict$Monad$IO
g_eval
g_proj 1
g_push 0
g_slide 1
g_updap 2, 1
g_unwind

g_globstart dict$Functor$List$ll1, 2
g_push 1
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal gm$cons_0_0
g_update 3
g_pop 2
g_unwind
g_label .1
g_uncons 2
g_push 1
g_push 3
g_pushglobal dict$Functor$List
g_eval
g_proj 0
g_push 0
g_slide 1
g_mkap 2
g_push 1
g_push 4
g_mkap 1
g_updcons 1, 2, 5
g_pop 4
g_return
g_jump .2
g_label .2

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
g_pushglobal dict$Foldable$List
g_eval
g_proj 0
g_push 0
g_slide 1
g_mkap 3
g_push 1
g_push 4
g_updap 2, 6
g_pop 5
g_unwind
g_label .2

g_globstart dict$Foldable$List$ll2, 3
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
g_push 1
g_push 5
g_push 5
g_mkap 2
g_push 4
g_pushglobal dict$Foldable$List
g_eval
g_proj 1
g_push 0
g_slide 1
g_updap 3, 6
g_pop 5
g_unwind
g_label .2

g_globstart nth_exn$ll1, 2
g_push 0
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal gm$abort
g_update 3
g_pop 2
g_unwind
g_label .1
g_uncons 2
g_pushint 0
g_push 4
g_pushglobal dict$Ord$Int
g_eval
g_proj 2
g_push 0
g_slide 1
g_mkap 2
g_eval
g_jumpcase .3, .4
g_label .3
g_pop 1
g_pushint 1
g_push 4
g_pushglobal dict$Ring$Int
g_eval
g_proj 2
g_push 0
g_slide 1
g_mkap 2
g_push 2
g_pushglobal nth_exn$ll1
g_updap 2, 5
g_pop 4
g_unwind
g_label .4
g_pop 1
g_update 4
g_pop 3
g_unwind
g_label .5
g_jump .2
g_label .2

g_globstart zip_with$ll1, 3
g_push 1
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal gm$cons_0_0
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
g_pushglobal gm$cons_0_0
g_update 6
g_pop 5
g_unwind
g_label .4
g_uncons 2
g_push 1
g_push 4
g_push 6
g_pushglobal zip_with$ll1
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

g_globstart dict$Monad$IO$ll2, 1
g_push 0
g_pushglobal gm$cons_0_2
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
g_pushglobal dict$Monad$IO$ll3
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

g_globstart mul_p$ll1, 2
g_pushglobal p
g_eval
g_push 2
g_push 2
g_pushglobal dict$Ring$Int
g_eval
g_proj 3
g_push 0
g_slide 1
g_mkap 2
g_eval
g_mod
g_update 3
g_pop 2
g_return

g_globstart add_p$ll1, 2
g_pushglobal p
g_eval
g_push 2
g_push 2
g_pushglobal dict$Ring$Int
g_eval
g_proj 1
g_push 0
g_slide 1
g_mkap 2
g_eval
g_mod
g_update 3
g_pop 2
g_return

g_globstart scanl$ll1, 4
g_push 3
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal gm$cons_0_0
g_update 5
g_pop 4
g_unwind
g_label .1
g_uncons 2
g_push 0
g_push 5
g_push 4
g_mkap 2
g_push 2
g_push 1
g_push 6
g_mkap 2
g_push 1
g_updcons 1, 2, 8
g_pop 7
g_return
g_jump .2
g_label .2

g_globstart sols$ll1, 1
g_push 0
g_pushglobal sols
g_pushglobal mul_p$ll1
g_pushglobal zip_with$ll1
g_mkap 3
g_pushglobal sum_p
g_updap 1, 2
g_pop 1
g_unwind

g_globstart sols$ll2, 2
g_push 0
g_push 2
g_updcons 1, 2, 3
g_pop 2
g_return

g_globstart main$ll1, 1
g_push 0
g_pushglobal sols
g_pushglobal nth_exn$ll1
g_mkap 2
g_pushglobal print$ll1
g_updap 1, 2
g_pop 1
g_unwind
