g_declare_globals gm$cons_0_0, 0, gm$cons_0_2, 2, gm$cons_0_4, 4, gm$cons_1_2, 2, gm$abort, 0, gm$lt, 2, gm$le, 2, gm$ge, 2, gm$gt, 2, gm$neg, 1, gm$add, 2, gm$sub, 2, gm$mul, 2, gm$seq, 2, gm$puti, 1, gm$geti, 1, dict$Ord$Int, 0, dict$Ring$Int, 0, dict$Monad$IO, 0, input, 0, prime, 0, fibs0, 0, fibs1, 0, main, 0, le$ll1, 1, lt$ll1, 1, add$ll1, 1, sub$ll1, 1, mul$ll1, 1, nth_exn$ll1, 2, zip_with$ll1, 3, bind$ll1, 1, dict$Monad$IO$ll1, 0, dict$Monad$IO$ll2, 1, dict$Monad$IO$ll3, 3, dict$Monad$IO$ll4, 2, io$ll1, 3, io$ll2, 2, print$ll1, 0, add_mod_prime$ll1, 2, main$ll1, 1
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
g_pushglobal gm$ge
g_pushglobal gm$gt
g_pushglobal gm$le
g_pushglobal gm$lt
g_push 0
g_push 2
g_push 4
g_push 6
g_updcons 0, 4, 5
g_pop 4
g_return

g_globstart dict$Ring$Int, 0
g_pushglobal gm$neg
g_pushglobal gm$add
g_pushglobal gm$sub
g_pushglobal gm$mul
g_push 0
g_push 2
g_push 4
g_push 6
g_updcons 0, 4, 5
g_pop 4
g_return

g_globstart dict$Monad$IO, 0
g_pushglobal dict$Monad$IO$ll2
g_pushglobal dict$Monad$IO$ll4
g_push 0
g_push 2
g_updcons 0, 2, 3
g_pop 2
g_return

g_globstart input, 0
g_pushglobal gm$cons_0_0
g_pushglobal gm$geti
g_pushglobal io$ll2
g_updap 2, 1
g_unwind

g_globstart prime, 0
g_pushint 39
g_pushint 1000000
g_pushint 1000000
g_pushglobal dict$Ring$Int
g_pushglobal mul$ll1
g_mkap 3
g_pushglobal dict$Ring$Int
g_pushglobal add$ll1
g_updap 3, 1
g_unwind

g_globstart fibs0, 0
g_pushglobal fibs1
g_pushint 0
g_updcons 1, 2, 1
g_return

g_globstart fibs1, 0
g_pushglobal fibs1
g_pushglobal fibs0
g_pushglobal add_mod_prime$ll1
g_pushglobal zip_with$ll1
g_mkap 3
g_pushint 1
g_updcons 1, 2, 1
g_return

g_globstart main, 0
g_pushglobal main$ll1
g_pushglobal input
g_pushglobal dict$Monad$IO
g_pushglobal bind$ll1
g_updap 3, 1
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

g_globstart mul$ll1, 1
g_push 0
g_eval
g_proj 3
g_update 2
g_pop 1
g_unwind

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
g_pushglobal le$ll1
g_mkap 3
g_eval
g_jumpcase .3, .4
g_label .3
g_pop 1
g_pushint 1
g_push 4
g_pushglobal dict$Ring$Int
g_pushglobal sub$ll1
g_mkap 3
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

g_globstart bind$ll1, 1
g_push 0
g_eval
g_proj 1
g_update 2
g_pop 1
g_unwind

g_globstart dict$Monad$IO$ll1, 0
g_pushglobal gm$cons_0_2
g_update 1
g_unwind

g_globstart dict$Monad$IO$ll2, 1
g_push 0
g_pushglobal dict$Monad$IO$ll1
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

g_globstart add_mod_prime$ll1, 2
g_push 1
g_push 1
g_pushglobal dict$Ring$Int
g_pushglobal add$ll1
g_mkap 3
g_pushglobal prime
g_push 1
g_pushglobal dict$Ord$Int
g_pushglobal lt$ll1
g_mkap 3
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal prime
g_push 1
g_pushglobal dict$Ring$Int
g_pushglobal sub$ll1
g_updap 3, 4
g_pop 3
g_unwind
g_label .1
g_pop 1
g_update 3
g_pop 2
g_unwind
g_label .2

g_globstart main$ll1, 1
g_push 0
g_pushglobal fibs0
g_pushglobal nth_exn$ll1
g_mkap 2
g_pushglobal print$ll1
g_updap 1, 2
g_pop 1
g_unwind
