g_declare_cafs gm$cons_0_0, gm$cons_1_0, gm$abort, dict$Eq$Int, dict$Ord$Int, dict$Ring$Int, dict$Monoid$List, dict$Foldable$List, dict$Monad$IO, input, psums, primes, main, dict$Monad$IO$ll1
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

g_globstart gm$cons_1_0, 0
g_updcons 1, 0, 1
g_return

g_globstart gm$cons_1_2, 2
g_updcons 1, 2, 1
g_return

g_globstart gm$abort, 0
g_abort

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

g_globstart dict$Eq$Int, 0
g_pushglobal gm$eq, 2
g_push 0
g_updcons 0, 1, 2
g_pop 1
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

g_globstart dict$Monoid$List, 0
g_pushglobal gm$cons_0_0, 0
g_pushglobal dict$Monoid$List$ll1, 2
g_push 0
g_push 2
g_updcons 0, 2, 3
g_pop 2
g_return

g_globstart dict$Foldable$List, 0
g_pushglobal dict$Foldable$List$ll1, 3
g_pushglobal dict$Foldable$List$ll2, 3
g_push 0
g_push 2
g_updcons 0, 2, 3
g_pop 2
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

g_globstart psums, 0
g_alloc 1
g_push 0
g_pushglobal psums$ll1, 4
g_updap 1, 1
g_pushint 0
g_pushglobal dict$Ring$Int, 0
g_push 2
g_updap 2, 2
g_pop 1
g_unwind

g_globstart primes, 0
g_pushglobal gm$cons_0_0, 0
g_pushint 4
g_cons 1, 2
g_pushint 2
g_cons 1, 2
g_pushglobal repeat$ll1, 1
g_mkap 1
g_pushint 5
g_cons 1, 2
g_pushglobal psums, 0
g_mkap 1
g_pushglobal sieve$ll2, 1
g_mkap 1
g_pushint 3
g_cons 1, 2
g_pushint 2
g_updcons 1, 2, 1
g_return

g_globstart main, 0
g_pushglobal main$ll1, 1
g_pushglobal input, 0
g_pushglobal dict$Monad$IO, 0
g_pushglobal bind$ll1, 1
g_updap 3, 1
g_unwind

g_globstart eq$ll1, 1
g_push 0
g_eval
g_proj 0
g_update 2
g_pop 1
g_unwind

g_globstart neq$ll1, 3
g_push 2
g_push 2
g_push 2
g_pushglobal eq$ll1, 1
g_mkap 3
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal gm$cons_1_0, 0
g_update 4
g_pop 3
g_unwind
g_label .1
g_pop 1
g_pushglobal gm$cons_0_0, 0
g_update 4
g_pop 3
g_unwind
g_label .2

g_globstart le$ll1, 1
g_push 0
g_eval
g_proj 2
g_update 2
g_pop 1
g_unwind

g_globstart append$ll1, 1
g_push 0
g_eval
g_proj 1
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

g_globstart foldr$ll1, 1
g_push 0
g_eval
g_proj 0
g_update 2
g_pop 1
g_unwind

g_globstart foldl$ll1, 1
g_push 0
g_eval
g_proj 1
g_update 2
g_pop 1
g_unwind

g_globstart dict$Monoid$List$ll1, 2
g_push 0
g_push 2
g_pushglobal gm$cons_1_2, 2
g_pushglobal dict$Foldable$List, 0
g_pushglobal foldr$ll1, 1
g_updap 4, 3
g_pop 2
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
g_pushglobal dict$Foldable$List, 0
g_pushglobal foldr$ll1, 1
g_mkap 4
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
g_pushglobal foldl$ll1, 1
g_updap 4, 6
g_pop 5
g_unwind
g_label .2

g_globstart nth_exn$ll1, 2
g_push 0
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal gm$abort, 0
g_update 3
g_pop 2
g_unwind
g_label .1
g_uncons 2
g_pushint 0
g_push 4
g_pushglobal dict$Ord$Int, 0
g_pushglobal le$ll1, 1
g_mkap 3
g_eval
g_jumpcase .3, .4
g_label .3
g_pop 1
g_pushint 1
g_push 4
g_pushglobal dict$Ring$Int, 0
g_pushglobal sub$ll1, 1
g_mkap 3
g_push 2
g_pushglobal nth_exn$ll1, 2
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

g_globstart bind$ll1, 1
g_push 0
g_eval
g_proj 1
g_update 2
g_pop 1
g_unwind

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

g_globstart repeat$ll1, 1
g_alloc 1
g_push 0
g_push 2
g_pushglobal dict$Monoid$List, 0
g_pushglobal append$ll1, 1
g_updap 3, 1
g_update 2
g_pop 1
g_unwind

g_globstart psums$ll1, 4
g_push 3
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal gm$cons_0_0, 0
g_update 5
g_pop 4
g_unwind
g_label .1
g_uncons 2
g_push 4
g_push 1
g_push 5
g_pushglobal add$ll1, 1
g_mkap 3
g_push 2
g_push 1
g_push 6
g_push 6
g_mkap 3
g_push 1
g_updcons 1, 2, 8
g_pop 7
g_return
g_jump .2
g_label .2

g_globstart filter$ll1, 3
g_push 2
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
g_push 1
g_push 4
g_mkap 1
g_push 1
g_push 4
g_mkap 1
g_eval
g_jumpcase .3, .4
g_label .3
g_pop 1
g_update 6
g_pop 5
g_unwind
g_label .4
g_pop 1
g_push 0
g_push 2
g_updcons 1, 2, 7
g_pop 6
g_return
g_jump .5
g_label .5
g_jump .2
g_label .2

g_globstart filter$ll2, 1
g_alloc 1
g_push 0
g_push 2
g_pushglobal filter$ll1, 3
g_updap 2, 1
g_update 2
g_pop 1
g_unwind

g_globstart sieve$ll1, 2
g_pushint 0
g_push 1
g_push 3
g_pushglobal gm$mod, 2
g_mkap 2
g_pushglobal dict$Eq$Int, 0
g_pushglobal neq$ll1, 3
g_updap 3, 3
g_pop 2
g_unwind

g_globstart sieve$ll2, 1
g_push 0
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal gm$abort, 0
g_update 2
g_pop 1
g_unwind
g_label .1
g_uncons 2
g_push 1
g_push 1
g_pushglobal sieve$ll1, 2
g_mkap 1
g_pushglobal filter$ll2, 1
g_mkap 2
g_pushglobal sieve$ll2, 1
g_mkap 1
g_push 1
g_updcons 1, 2, 4
g_pop 3
g_return
g_jump .2
g_label .2

g_globstart main$ll1, 1
g_push 0
g_pushglobal primes, 0
g_pushglobal nth_exn$ll1, 2
g_mkap 2
g_pushglobal print$ll1, 1
g_updap 1, 2
g_pop 1
g_unwind
