g_declare_cafs gm$cons_0_0, dict$Eq$Int, dict$Ord$Int, dict$Ring$Int, dict$Monoid$Int, dict$Functor$List, dict$Foldable$List, dict$Monad$IO, input, ints, main, print$ll1
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

g_globstart gm$eq, 2
g_push 1
g_eval
g_push 1
g_eval
g_eqv
g_update 3
g_pop 2
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

g_globstart dict$Eq$Int, 0
g_pushglobal gm$eq, 2
g_updcons 0, 1, 1
g_return

g_globstart dict$Ord$Int, 0
g_pushglobal gm$lt, 2
g_pushglobal gm$le, 2
g_pushglobal gm$gt, 2
g_pushglobal gm$ge, 2
g_updcons 0, 4, 1
g_return

g_globstart dict$Ring$Int, 0
g_pushglobal gm$mul, 2
g_pushglobal gm$sub, 2
g_pushglobal gm$add, 2
g_pushglobal gm$neg, 1
g_updcons 0, 4, 1
g_return

g_globstart dict$Monoid$Int, 0
g_pushglobal gm$add, 2
g_pushint 0
g_updcons 0, 2, 1
g_return

g_globstart dict$Functor$List, 0
g_pushglobal dict$Functor$List$ll1, 2
g_updcons 0, 1, 1
g_return

g_globstart dict$Foldable$List, 0
g_pushglobal dict$Foldable$List$ll2, 3
g_pushglobal dict$Foldable$List$ll1, 3
g_updcons 0, 2, 1
g_return

g_globstart dict$Monad$IO, 0
g_pushglobal dict$Monad$IO$ll4, 2
g_pushglobal dict$Monad$IO$ll2, 1
g_updcons 0, 2, 1
g_return

g_globstart input, 0
g_pushglobal gm$cons_0_0, 0
g_pushglobal gm$geti, 1
g_pushglobal io$ll1, 3
g_updap 2, 1
g_unwind

g_globstart ints, 0
g_alloc 1
g_push 0
g_pushglobal ints$ll1, 2
g_updap 1, 1
g_pushint 1
g_push 1
g_updap 1, 2
g_pop 1
g_unwind

g_globstart main, 0
g_pushglobal main$ll1, 1
g_pushglobal input, 0
g_pushglobal dict$Monad$IO, 0
g_eval
g_proj 1
g_push 0
g_slide 1
g_updap 2, 1
g_unwind

g_globstart foldMap$ll1, 3
g_push 2
g_push 2
g_mkap 1
g_push 1
g_eval
g_proj 1
g_push 0
g_slide 1
g_updap 1, 4
g_pop 3
g_unwind

g_globstart length$ll1, 1
g_pushint 1
g_update 2
g_pop 1
g_return

g_globstart dict$Monoid$List$ll1, 2
g_push 0
g_push 2
g_pushglobal gm$cons_1_2, 2
g_pushglobal dict$Foldable$List, 0
g_eval
g_proj 0
g_push 0
g_slide 1
g_updap 3, 3
g_pop 2
g_unwind

g_globstart dict$Functor$List$ll1, 2
g_push 1
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal gm$cons_0_0, 0
g_update 3
g_pop 2
g_unwind
g_label .1
g_uncons 2
g_push 1
g_push 3
g_pushglobal dict$Functor$List, 0
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
g_pushglobal dict$Foldable$List, 0
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
g_pushglobal dict$Foldable$List, 0
g_eval
g_proj 1
g_push 0
g_slide 1
g_updap 3, 6
g_pop 5
g_unwind
g_label .2

g_globstart take$ll1, 2
g_pushint 0
g_push 1
g_pushglobal dict$Ord$Int, 0
g_eval
g_proj 2
g_push 0
g_slide 1
g_mkap 2
g_eval
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
g_pushglobal dict$Ring$Int, 0
g_eval
g_proj 2
g_push 0
g_slide 1
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

g_globstart replicate$ll1, 2
g_pushint 0
g_push 1
g_pushglobal dict$Ord$Int, 0
g_eval
g_proj 2
g_push 0
g_slide 1
g_mkap 2
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_push 1
g_pushint 1
g_push 2
g_pushglobal dict$Ring$Int, 0
g_eval
g_proj 2
g_push 0
g_slide 1
g_mkap 2
g_pushglobal replicate$ll1, 2
g_mkap 2
g_push 2
g_updcons 1, 2, 3
g_pop 2
g_return
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

g_globstart diff$ll1, 2
g_push 0
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal gm$cons_0_0, 0
g_update 3
g_pop 2
g_unwind
g_label .1
g_uncons 2
g_push 3
g_eval
g_jumpcase .3, .4
g_label .3
g_pop 3
g_update 2
g_pop 1
g_unwind
g_label .4
g_uncons 2
g_push 0
g_push 3
g_pushglobal dict$Ord$Int, 0
g_eval
g_proj 3
g_push 0
g_slide 1
g_mkap 2
g_eval
g_jumpcase .6, .7
g_label .6
g_pop 1
g_push 0
g_push 3
g_pushglobal dict$Eq$Int, 0
g_eval
g_proj 0
g_push 0
g_slide 1
g_mkap 2
g_eval
g_jumpcase .9, .10
g_label .9
g_pop 1
g_push 1
g_push 5
g_pushglobal diff$ll1, 2
g_updap 2, 7
g_pop 6
g_unwind
g_label .10
g_pop 1
g_push 1
g_push 4
g_pushglobal diff$ll1, 2
g_updap 2, 7
g_pop 6
g_unwind
g_label .11
g_jump .8
g_label .7
g_pop 1
g_push 5
g_push 4
g_pushglobal diff$ll1, 2
g_mkap 2
g_push 3
g_updcons 1, 2, 7
g_pop 6
g_return
g_jump .8
g_label .8
g_jump .5
g_label .5
g_jump .2
g_label .2

g_globstart ints$ll1, 2
g_pushint 1
g_push 2
g_pushglobal dict$Ring$Int, 0
g_eval
g_proj 1
g_push 0
g_slide 1
g_mkap 2
g_push 1
g_mkap 1
g_push 2
g_updcons 1, 2, 3
g_pop 2
g_return

g_globstart solve_aux$ll1, 3
g_pushglobal gm$cons_0_0, 0
g_push 3
g_push 2
g_pushglobal dict$Ring$Int, 0
g_eval
g_proj 1
g_push 0
g_slide 1
g_mkap 2
g_cons 1, 2
g_push 1
g_cons 1, 2
g_push 3
g_push 2
g_pushglobal dict$Ring$Int, 0
g_eval
g_proj 2
g_push 0
g_slide 1
g_mkap 2
g_cons 1, 2
g_push 2
g_pushglobal diff$ll1, 2
g_updap 2, 4
g_pop 3
g_unwind

g_globstart solve_aux$ll2, 2
g_pushglobal ints, 0
g_push 1
g_push 3
g_pushglobal solve_aux$ll1, 3
g_mkap 1
g_pushglobal zip_with$ll1, 3
g_mkap 3
g_pushglobal solve_aux$ll3, 1
g_mkap 1
g_push 2
g_pushglobal gm$cons_1_2, 2
g_mkap 1
g_pushglobal dict$Functor$List, 0
g_eval
g_proj 0
g_push 0
g_slide 1
g_updap 2, 3
g_pop 2
g_unwind

g_globstart solve_aux$ll3, 1
g_push 0
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal gm$cons_0_0, 0
g_pushglobal gm$cons_0_0, 0
g_updcons 1, 2, 2
g_pop 1
g_return
g_jump .2
g_label .1
g_uncons 2
g_pushglobal gm$cons_0_0, 0
g_pushglobal dict$Monoid$List$ll1, 2
g_push 0
g_push 2
g_cons 0, 2
g_slide 2
g_push 2
g_pushglobal solve_aux$ll2, 2
g_mkap 1
g_push 2
g_push 2
g_eval
g_proj 0
g_push 0
g_slide 1
g_push 2
g_push 4
g_pushglobal foldMap$ll1, 3
g_mkap 2
g_pushglobal dict$Foldable$List, 0
g_eval
g_proj 0
g_push 0
g_slide 1
g_updap 3, 6
g_pop 5
g_unwind
g_label .2

g_globstart main$ll1, 1
g_pushglobal length$ll1, 1
g_pushglobal ints, 0
g_push 2
g_pushglobal take$ll1, 2
g_mkap 2
g_push 2
g_pushglobal replicate$ll1, 2
g_mkap 2
g_pushglobal solve_aux$ll3, 1
g_mkap 1
g_pushglobal dict$Monoid$Int, 0
g_eval
g_proj 0
g_push 0
g_slide 1
g_push 2
g_pushglobal dict$Monoid$Int, 0
g_pushglobal foldMap$ll1, 3
g_mkap 2
g_pushglobal dict$Foldable$List, 0
g_eval
g_proj 0
g_push 0
g_slide 1
g_mkap 3
g_slide 1
g_pushglobal print$ll1, 0
g_updap 1, 2
g_pop 1
g_unwind
