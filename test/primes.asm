g_declare_globals C.0.0, 0, C.0.2, 2, C.0.4, 4, C.1.0, 0, C.1.2, 2, B.abort, 0, B.eq, 2, B.le, 2, B.neg, 1, B.add, 2, B.sub, 2, B.mul, 2, B.mod, 2, B.seq, 2, B.puti, 1, B.geti, 1, ringInt, 0, print, 0, input, 0, psums, 0, sieve, 1, primes, 0, main, 0, monoidList.empty, 0, monoidList.append.L1, 2, foldableList.foldr.L1, 3, nth_exn.L1, 2, monadIO.bind.L1, 3, io.L1, 3, io.L2, 2, psums.L1, 4, filter.L1, 3, sieve.L1, 2, main.L1, 1
g_declare_main main

g_globstart C.0.0, 0
g_updcons 0, 0, 1
g_return

g_globstart C.0.2, 2
g_updcons 0, 2, 1
g_return

g_globstart C.0.4, 4
g_updcons 0, 4, 1
g_return

g_globstart C.1.0, 0
g_updcons 1, 0, 1
g_return

g_globstart C.1.2, 2
g_updcons 1, 2, 1
g_return

g_globstart B.abort, 0
g_abort

g_globstart B.eq, 2
g_push 1
g_eval
g_push 1
g_eval
g_eqv
g_update 3
g_pop 2
g_return

g_globstart B.le, 2
g_push 1
g_eval
g_push 1
g_eval
g_leq
g_update 3
g_pop 2
g_return

g_globstart B.neg, 1
g_eval
g_neg
g_update 1
g_return

g_globstart B.add, 2
g_push 1
g_eval
g_push 1
g_eval
g_add
g_update 3
g_pop 2
g_return

g_globstart B.sub, 2
g_push 1
g_eval
g_push 1
g_eval
g_sub
g_update 3
g_pop 2
g_return

g_globstart B.mul, 2
g_push 1
g_eval
g_push 1
g_eval
g_mul
g_update 3
g_pop 2
g_return

g_globstart B.mod, 2
g_push 1
g_eval
g_push 1
g_eval
g_mod
g_update 3
g_pop 2
g_return

g_globstart B.seq, 2
g_eval
g_pop 1
g_update 1
g_unwind

g_globstart B.puti, 1
g_eval
g_print
g_updcons 0, 0, 1
g_return

g_globstart B.geti, 1
g_pop 1
g_input
g_update 1
g_return

g_globstart ringInt, 0
g_pushglobal B.mul
g_pushglobal B.sub
g_pushglobal B.add
g_pushglobal B.neg
g_updcons 0, 4, 1
g_return

g_globstart print, 0
g_pushglobal B.puti
g_pushglobal io.L2
g_updap 1, 1
g_unwind

g_globstart input, 0
g_pushglobal C.0.0
g_pushglobal B.geti
g_pushglobal io.L1
g_updap 2, 1
g_unwind

g_globstart psums, 0
g_alloc 1
g_push 0
g_pushglobal psums.L1
g_updap 1, 1
g_pushint 0
g_pushglobal ringInt
g_push 2
g_updap 2, 2
g_pop 1
g_unwind

g_globstart sieve, 1
g_push 0
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal B.abort
g_update 2
g_pop 1
g_unwind
g_label .1
g_uncons 2
g_push 0
g_pushglobal sieve.L1
g_mkap 1
g_alloc 1
g_push 0
g_push 2
g_pushglobal filter.L1
g_updap 2, 1
g_push 3
g_push 1
g_mkap 1
g_slide 1
g_slide 1
g_pushglobal sieve
g_mkap 1
g_push 1
g_updcons 1, 2, 4
g_pop 3
g_return
g_jump .2
g_label .2

g_globstart primes, 0
g_pushglobal C.0.0
g_pushint 4
g_cons 1, 2
g_pushint 2
g_cons 1, 2
g_alloc 1
g_pushglobal monoidList.append.L1
g_pushglobal monoidList.empty
g_cons 0, 2
g_push 1
g_push 3
g_push 2
g_eval
g_proj 1
g_push 0
g_slide 1
g_mkap 2
g_slide 1
g_update 1
g_push 0
g_slide 1
g_slide 1
g_pushint 5
g_cons 1, 2
g_pushglobal psums
g_mkap 1
g_pushglobal sieve
g_mkap 1
g_pushint 3
g_cons 1, 2
g_pushint 2
g_updcons 1, 2, 1
g_return

g_globstart main, 0
g_pushglobal main.L1
g_pushglobal input
g_pushglobal monadIO.bind.L1
g_updap 2, 1
g_unwind

g_globstart monoidList.empty, 0
g_pushglobal C.0.0
g_update 1
g_unwind

g_globstart monoidList.append.L1, 2
g_push 0
g_push 2
g_pushglobal C.1.2
g_pushglobal foldableList.foldr.L1
g_updap 3, 3
g_pop 2
g_unwind

g_globstart foldableList.foldr.L1, 3
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
g_pushglobal foldableList.foldr.L1
g_mkap 3
g_push 1
g_push 4
g_updap 2, 6
g_pop 5
g_unwind
g_label .2

g_globstart nth_exn.L1, 2
g_push 0
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal B.abort
g_update 3
g_pop 2
g_unwind
g_label .1
g_uncons 2
g_pushint 0
g_push 4
g_eval
g_leq
g_jumpcase .3, .4
g_label .3
g_pop 1
g_pushint 1
g_push 4
g_pushglobal B.sub
g_mkap 2
g_push 2
g_pushglobal nth_exn.L1
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

g_globstart monadIO.bind.L1, 3
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

g_globstart io.L1, 3
g_push 1
g_push 1
g_mkap 1
g_push 3
g_push 1
g_cons 0, 2
g_push 1
g_pushglobal B.seq
g_updap 2, 5
g_pop 4
g_unwind

g_globstart io.L2, 2
g_push 1
g_push 1
g_pushglobal io.L1
g_updap 2, 3
g_pop 2
g_unwind

g_globstart psums.L1, 4
g_push 3
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal C.0.0
g_update 5
g_pop 4
g_unwind
g_label .1
g_uncons 2
g_push 4
g_push 1
g_push 5
g_eval
g_proj 1
g_push 0
g_slide 1
g_mkap 2
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

g_globstart filter.L1, 3
g_push 2
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal C.0.0
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

g_globstart sieve.L1, 2
g_push 0
g_push 2
g_pushglobal B.mod
g_mkap 2
g_pushint 0
g_push 1
g_eval
g_eqv
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal C.1.0
g_update 4
g_pop 3
g_unwind
g_label .1
g_pop 1
g_pushglobal C.0.0
g_update 4
g_pop 3
g_unwind
g_label .2

g_globstart main.L1, 1
g_push 0
g_pushglobal primes
g_pushglobal nth_exn.L1
g_mkap 2
g_pushglobal print
g_updap 1, 2
g_pop 1
g_unwind
