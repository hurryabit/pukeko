g_declare_cafs gm$cons_0_0, gm$cons_1_0, gm$abort, dict$Monad$IO, input, nats, infinity, main, print$ll1
g_declare_main main

g_globstart gm$cons_0_0, 0
g_updcons 0, 0, 1
g_return

g_globstart gm$cons_0_2, 2
g_updcons 0, 2, 1
g_return

g_globstart gm$cons_1_0, 0
g_updcons 1, 0, 1
g_return

g_globstart gm$cons_1_2, 2
g_updcons 1, 2, 1
g_return

g_globstart gm$cons_1_5, 5
g_updcons 1, 5, 1
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

g_globstart gm$gt, 2
g_push 1
g_eval
g_push 1
g_eval
g_gtr
g_update 3
g_pop 2
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

g_globstart nats, 0
g_alloc 1
g_push 0
g_pushglobal nats$ll1, 2
g_updap 1, 1
g_pushint 0
g_push 1
g_updap 1, 2
g_pop 1
g_unwind

g_globstart infinity, 0
g_pushint 1000000000
g_update 1
g_return

g_globstart main, 0
g_pushglobal main$ll6, 1
g_pushglobal input, 0
g_pushglobal dict$Monad$IO$ll3, 3
g_updap 2, 1
g_unwind

g_globstart replicate$ll1, 2
g_pushint 0
g_push 1
g_eval
g_leq
g_jumpcase .0, .1
g_label .0
g_pop 1
g_push 1
g_pushint 1
g_push 2
g_pushglobal gm$sub, 2
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

g_globstart nats$ll1, 2
g_pushint 1
g_push 2
g_pushglobal gm$add, 2
g_mkap 2
g_push 1
g_mkap 1
g_push 2
g_updcons 1, 2, 3
g_pop 2
g_return

g_globstart pair$ll1, 2
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
g_eval
g_jumpcase .3, .4
g_label .3
g_pop 4
g_update 1
g_unwind
g_label .4
g_uncons 2
g_push 1
g_push 5
g_pushglobal pair$ll1, 2
g_mkap 2
g_push 1
g_push 4
g_push 7
g_mkap 2
g_updcons 1, 2, 7
g_pop 6
g_return
g_jump .5
g_label .5
g_jump .2
g_label .2

g_globstart single$ll1, 2
g_pushglobal gm$cons_0_0, 0
g_pushglobal gm$cons_0_0, 0
g_push 3
g_push 3
g_push 4
g_updcons 1, 5, 3
g_pop 2
g_return

g_globstart combine$ll1, 3
g_push 1
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal gm$abort, 0
g_update 4
g_pop 3
g_unwind
g_label .1
g_uncons 5
g_push 7
g_eval
g_jumpcase .3, .4
g_label .3
g_pop 1
g_pushglobal gm$abort, 0
g_update 9
g_pop 8
g_unwind
g_label .4
g_uncons 5
g_push 12
g_push 12
g_push 4
g_push 10
g_push 14
g_mkap 2
g_push 4
g_push 9
g_updcons 1, 5, 14
g_pop 13
g_return
g_jump .5
g_label .5
g_jump .2
g_label .2

g_globstart build$ll1, 3
g_push 2
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal gm$abort, 0
g_update 4
g_pop 3
g_unwind
g_label .1
g_uncons 2
g_push 1
g_eval
g_jumpcase .3, .4
g_label .3
g_pop 1
g_update 5
g_pop 4
g_unwind
g_label .4
g_uncons 2
g_push 6
g_push 5
g_pushglobal combine$ll1, 3
g_mkap 1
g_pushglobal pair$ll1, 2
g_mkap 2
g_push 6
g_updap 1, 8
g_pop 7
g_unwind
g_label .5
g_jump .2
g_label .2

g_globstart query$ll1, 6
g_push 5
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_update 6
g_pop 5
g_unwind
g_label .1
g_uncons 5
g_push 0
g_push 9
g_pushglobal gm$lt, 2
g_mkap 2
g_push 2
g_push 9
g_pushglobal gm$gt, 2
g_mkap 2
g_push 1
g_eval
g_jumpcase .3, .4
g_label .3
g_pop 1
g_push 0
g_eval
g_slide 0
g_jump .5
g_label .4
g_pop 1
g_pushglobal gm$cons_1_0, 0
g_eval
g_slide 0
g_jump .5
g_label .5
g_slide 2
g_jumpcase .6, .7
g_label .6
g_pop 1
g_push 0
g_push 8
g_pushglobal gm$le, 2
g_mkap 2
g_push 9
g_push 3
g_pushglobal gm$le, 2
g_mkap 2
g_push 1
g_eval
g_jumpcase .9, .10
g_label .9
g_pop 1
g_pushglobal gm$cons_0_0, 0
g_eval
g_slide 0
g_jump .11
g_label .10
g_pop 1
g_push 0
g_eval
g_slide 0
g_jump .11
g_label .11
g_slide 2
g_jumpcase .12, .13
g_label .12
g_pop 1
g_push 4
g_push 10
g_mkap 1
g_push 4
g_push 11
g_mkap 1
g_push 8
g_updap 2, 12
g_pop 11
g_unwind
g_label .13
g_pop 3
g_update 9
g_pop 8
g_unwind
g_label .14
g_jump .8
g_label .7
g_pop 6
g_update 6
g_pop 5
g_unwind
g_label .8
g_jump .2
g_label .2

g_globstart min$ll1, 2
g_push 1
g_eval
g_push 1
g_eval
g_leq
g_jumpcase .0, .1
g_label .0
g_pop 2
g_update 1
g_unwind
g_label .1
g_pop 1
g_update 2
g_pop 1
g_unwind
g_label .2

g_globstart main$ll1, 3
g_alloc 1
g_push 0
g_push 4
g_push 4
g_pushglobal min$ll1, 2
g_pushglobal infinity, 0
g_pushglobal query$ll1, 6
g_updap 5, 1
g_push 1
g_push 1
g_mkap 1
g_slide 1
g_push 0
g_pushglobal print$ll1, 0
g_updap 1, 5
g_pop 4
g_unwind

g_globstart main$ll2, 2
g_push 1
g_push 1
g_pushglobal main$ll1, 3
g_mkap 2
g_push 0
g_pushglobal input, 0
g_pushglobal dict$Monad$IO$ll3, 3
g_updap 2, 4
g_pop 3
g_unwind

g_globstart main$ll3, 1
g_pushglobal gm$cons_0_0, 0
g_pushglobal gm$cons_0_2, 2
g_updap 1, 2
g_pop 1
g_unwind

g_globstart main$ll4, 2
g_alloc 1
g_push 0
g_pushglobal min$ll1, 2
g_pushglobal build$ll1, 3
g_updap 2, 1
g_push 2
g_pushglobal nats, 0
g_pushglobal single$ll1, 2
g_pushglobal zip_with$ll1, 3
g_mkap 3
g_push 1
g_mkap 1
g_slide 1
g_push 0
g_pushglobal main$ll2, 2
g_mkap 1
g_push 0
g_pushglobal input, 0
g_pushglobal dict$Monad$IO$ll3, 3
g_mkap 2
g_slide 1
g_push 0
g_push 3
g_pushglobal replicate$ll1, 2
g_mkap 2
g_pushglobal dict$Monad$IO, 0
g_pushglobal sequence$ll3, 2
g_mkap 2
g_slide 1
g_pushglobal main$ll3, 1
g_push 1
g_pushglobal dict$Monad$IO$ll3, 3
g_updap 2, 5
g_pop 4
g_unwind

g_globstart main$ll5, 2
g_pushglobal input, 0
g_push 1
g_pushglobal replicate$ll1, 2
g_mkap 2
g_pushglobal dict$Monad$IO, 0
g_pushglobal sequence$ll3, 2
g_mkap 2
g_push 2
g_pushglobal main$ll4, 2
g_mkap 1
g_push 0
g_push 2
g_pushglobal dict$Monad$IO$ll3, 3
g_updap 2, 5
g_pop 4
g_unwind

g_globstart main$ll6, 1
g_push 0
g_pushglobal main$ll5, 2
g_mkap 1
g_push 0
g_pushglobal input, 0
g_pushglobal dict$Monad$IO$ll3, 3
g_updap 2, 3
g_pop 2
g_unwind
