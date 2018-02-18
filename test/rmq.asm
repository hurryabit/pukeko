g_declare_cafs gm$cons_0_0, gm$cons_1_0, gm$abort, dict$Ord$Int, dict$Ring$Int, dict$Monad$IO, input, nats, infinity, main, dict$Monad$IO$ll1
g_declare_main main

g_globstart gm$cons_0_0, 0
g_updcons 0, 0, 1
g_return

g_globstart gm$cons_0_2, 2
g_updcons 0, 2, 1
g_return

g_globstart gm$cons_0_4, 4
g_updcons 0, 4, 1
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

g_globstart dict$Ord$Int, 0
g_pushglobal gm$ge, 2
g_pushglobal gm$gt, 2
g_pushglobal gm$le, 2
g_pushglobal gm$lt, 2
g_push 0
g_push 2
g_push 4
g_push 6
g_updcons 0, 4, 5
g_pop 4
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

g_globstart dict$Monad$IO, 0
g_pushglobal dict$Monad$IO$ll2, 1
g_pushglobal dict$Monad$IO$ll4, 2
g_push 0
g_push 2
g_updcons 0, 2, 3
g_pop 2
g_return

g_globstart input, 0
g_pushglobal gm$cons_0_0, 0
g_pushglobal gm$geti, 1
g_pushglobal io$ll2, 2
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
g_pushglobal dict$Monad$IO, 0
g_pushglobal bind$ll1, 1
g_updap 3, 1
g_unwind

g_globstart conj$ll1, 2
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
g_pop 2
g_update 1
g_unwind
g_label .2

g_globstart disj$ll1, 2
g_push 0
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 2
g_update 1
g_unwind
g_label .1
g_pop 1
g_pushglobal gm$cons_1_0, 0
g_update 3
g_pop 2
g_unwind
g_label .2

g_globstart gt$ll1, 1
g_push 0
g_eval
g_proj 1
g_update 2
g_pop 1
g_unwind

g_globstart le$ll1, 1
g_push 0
g_eval
g_proj 2
g_update 2
g_pop 1
g_unwind

g_globstart lt$ll1, 1
g_push 0
g_eval
g_proj 3
g_update 2
g_pop 1
g_unwind

g_globstart add$ll1, 1
g_push 0
g_eval
g_proj 1
g_update 2
g_pop 1
g_unwind

g_globstart sub$ll1, 1
g_push 0
g_eval
g_proj 2
g_update 2
g_pop 1
g_unwind

g_globstart replicate$ll1, 2
g_pushint 0
g_push 1
g_pushglobal dict$Ord$Int, 0
g_pushglobal le$ll1, 1
g_mkap 3
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_push 1
g_pushint 1
g_push 2
g_pushglobal dict$Ring$Int, 0
g_pushglobal sub$ll1, 1
g_mkap 3
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

g_globstart pure$ll1, 1
g_push 0
g_eval
g_proj 0
g_update 2
g_pop 1
g_unwind

g_globstart bind$ll1, 1
g_push 0
g_eval
g_proj 1
g_update 2
g_pop 1
g_unwind

g_globstart sequence$ll1, 3
g_push 2
g_push 2
g_cons 1, 2
g_push 1
g_pushglobal pure$ll1, 1
g_updap 2, 4
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
g_pushglobal bind$ll1, 1
g_updap 3, 4
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
g_pushglobal pure$ll1, 1
g_updap 2, 3
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
g_pushglobal bind$ll1, 1
g_updap 3, 5
g_pop 4
g_unwind
g_label .2

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

g_globstart print$ll1, 1
g_push 0
g_pushglobal gm$puti, 1
g_pushglobal io$ll2, 2
g_updap 2, 2
g_pop 1
g_unwind

g_globstart nats$ll1, 2
g_pushint 1
g_push 2
g_pushglobal dict$Ring$Int, 0
g_pushglobal add$ll1, 1
g_mkap 3
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

g_globstart build$ll2, 2
g_alloc 1
g_push 0
g_push 2
g_pushglobal build$ll1, 3
g_updap 2, 1
g_push 2
g_pushglobal nats, 0
g_pushglobal single$ll1, 2
g_pushglobal zip_with$ll1, 3
g_mkap 3
g_push 1
g_updap 1, 4
g_pop 3
g_unwind

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
g_push 1
g_push 8
g_pushglobal dict$Ord$Int, 0
g_pushglobal gt$ll1, 1
g_mkap 3
g_push 1
g_push 10
g_pushglobal dict$Ord$Int, 0
g_pushglobal lt$ll1, 1
g_mkap 3
g_pushglobal disj$ll1, 2
g_mkap 2
g_eval
g_jumpcase .3, .4
g_label .3
g_pop 1
g_push 8
g_push 2
g_pushglobal dict$Ord$Int, 0
g_pushglobal le$ll1, 1
g_mkap 3
g_push 1
g_push 9
g_pushglobal dict$Ord$Int, 0
g_pushglobal le$ll1, 1
g_mkap 3
g_pushglobal conj$ll1, 2
g_mkap 2
g_eval
g_jumpcase .6, .7
g_label .6
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
g_label .7
g_pop 3
g_update 9
g_pop 8
g_unwind
g_label .8
g_jump .5
g_label .4
g_pop 6
g_update 6
g_pop 5
g_unwind
g_label .5
g_jump .2
g_label .2

g_globstart query$ll2, 4
g_alloc 1
g_push 0
g_push 5
g_push 5
g_push 5
g_push 5
g_pushglobal query$ll1, 6
g_updap 5, 1
g_update 5
g_pop 4
g_unwind

g_globstart min$ll1, 2
g_push 1
g_push 1
g_pushglobal dict$Ord$Int, 0
g_pushglobal le$ll1, 1
g_mkap 3
g_eval
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

g_globstart replicate_io$ll1, 2
g_push 1
g_push 1
g_pushglobal replicate$ll1, 2
g_mkap 2
g_pushglobal dict$Monad$IO, 0
g_pushglobal sequence$ll3, 2
g_updap 2, 3
g_pop 2
g_unwind

g_globstart main$ll1, 3
g_push 0
g_push 3
g_push 3
g_pushglobal min$ll1, 2
g_pushglobal infinity, 0
g_pushglobal query$ll2, 4
g_mkap 5
g_push 0
g_pushglobal print$ll1, 1
g_updap 1, 5
g_pop 4
g_unwind

g_globstart main$ll2, 2
g_push 1
g_push 1
g_pushglobal main$ll1, 3
g_mkap 2
g_pushglobal input, 0
g_pushglobal dict$Monad$IO, 0
g_pushglobal bind$ll1, 1
g_updap 3, 3
g_pop 2
g_unwind

g_globstart main$ll3, 1
g_pushglobal gm$cons_0_0, 0
g_pushglobal dict$Monad$IO, 0
g_pushglobal pure$ll1, 1
g_updap 2, 2
g_pop 1
g_unwind

g_globstart main$ll4, 2
g_push 1
g_pushglobal min$ll1, 2
g_pushglobal build$ll2, 2
g_mkap 2
g_pushglobal main$ll3, 1
g_push 1
g_pushglobal main$ll2, 2
g_mkap 1
g_pushglobal input, 0
g_pushglobal dict$Monad$IO, 0
g_pushglobal bind$ll1, 1
g_mkap 3
g_push 3
g_pushglobal replicate_io$ll1, 2
g_mkap 2
g_pushglobal dict$Monad$IO, 0
g_pushglobal bind$ll1, 1
g_updap 3, 4
g_pop 3
g_unwind

g_globstart main$ll5, 2
g_push 1
g_pushglobal main$ll4, 2
g_mkap 1
g_pushglobal input, 0
g_push 2
g_pushglobal replicate_io$ll1, 2
g_mkap 2
g_pushglobal dict$Monad$IO, 0
g_pushglobal bind$ll1, 1
g_updap 3, 3
g_pop 2
g_unwind

g_globstart main$ll6, 1
g_push 0
g_pushglobal main$ll5, 2
g_mkap 1
g_pushglobal input, 0
g_pushglobal dict$Monad$IO, 0
g_pushglobal bind$ll1, 1
g_updap 3, 2
g_pop 1
g_unwind
