g_declare_globals C.0.0, 0, C.0.1, 1, C.0.2, 2, C.0.4, 4, C.1.0, 0, C.1.2, 2, B.abort, 0, B.eq, 2, B.lt, 2, B.le, 2, B.ge, 2, B.gt, 2, B.neg, 1, B.add, 2, B.sub, 2, B.mul, 2, B.mod, 2, B.seq, 2, B.puti, 1, B.geti, 1, eqInt, 0, ordInt, 0, ringInt, 0, foldableList, 0, monadIO, 0, input, 0, psums, 0, primes, 0, main, 0, eq.L1, 1, neq.L1, 3, le.L1, 1, append.L1, 1, add.L1, 1, sub.L1, 1, foldr.L1, 1, foldl.L1, 1, monoidList.L1, 0, monoidList.empty.L1, 0, monoidList.append.L1, 2, foldableList.foldr.L1, 3, foldableList.foldl.L1, 3, nth_exn.L1, 2, bind.L1, 1, monadIO.pure.L1, 0, monadIO.pure.L2, 1, monadIO.bind.L1, 3, monadIO.bind.L2, 2, io.L1, 3, io.L2, 2, print.L1, 0, repeat.L1, 1, psums.L1, 4, filter.L1, 3, filter.L2, 1, sieve.L1, 2, sieve.L2, 1, main.L1, 1
g_declare_main main

g_globstart C.0.0, 0
g_updcons 0, 0, 1
g_return

g_globstart C.0.1, 1
g_updcons 0, 1, 1
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

g_globstart B.lt, 2
g_push 1
g_eval
g_push 1
g_eval
g_les
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

g_globstart B.ge, 2
g_push 1
g_eval
g_push 1
g_eval
g_geq
g_update 3
g_pop 2
g_return

g_globstart B.gt, 2
g_push 1
g_eval
g_push 1
g_eval
g_gtr
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

g_globstart eqInt, 0
g_pushglobal B.eq
g_updcons 0, 1, 1
g_return

g_globstart ordInt, 0
g_pushglobal B.lt
g_pushglobal B.le
g_pushglobal B.gt
g_pushglobal B.ge
g_updcons 0, 4, 1
g_return

g_globstart ringInt, 0
g_pushglobal B.mul
g_pushglobal B.sub
g_pushglobal B.add
g_pushglobal B.neg
g_updcons 0, 4, 1
g_return

g_globstart foldableList, 0
g_pushglobal foldableList.foldl.L1
g_pushglobal foldableList.foldr.L1
g_updcons 0, 2, 1
g_return

g_globstart monadIO, 0
g_pushglobal monadIO.bind.L2
g_pushglobal monadIO.pure.L2
g_updcons 0, 2, 1
g_return

g_globstart input, 0
g_pushglobal C.0.0
g_pushglobal B.geti
g_pushglobal io.L2
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

g_globstart primes, 0
g_pushglobal C.0.0
g_pushint 4
g_cons 1, 2
g_pushint 2
g_cons 1, 2
g_pushglobal repeat.L1
g_mkap 1
g_pushint 5
g_cons 1, 2
g_pushglobal psums
g_mkap 1
g_pushglobal sieve.L2
g_mkap 1
g_pushint 3
g_cons 1, 2
g_pushint 2
g_updcons 1, 2, 1
g_return

g_globstart main, 0
g_pushglobal main.L1
g_pushglobal input
g_pushglobal monadIO
g_pushglobal bind.L1
g_updap 3, 1
g_unwind

g_globstart eq.L1, 1
g_push 0
g_eval
g_proj 0
g_update 2
g_pop 1
g_unwind

g_globstart neq.L1, 3
g_push 2
g_push 2
g_push 2
g_pushglobal eq.L1
g_mkap 3
g_eval
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

g_globstart le.L1, 1
g_push 0
g_eval
g_proj 2
g_update 2
g_pop 1
g_unwind

g_globstart append.L1, 1
g_push 0
g_eval
g_proj 1
g_update 2
g_pop 1
g_unwind

g_globstart add.L1, 1
g_push 0
g_eval
g_proj 1
g_update 2
g_pop 1
g_unwind

g_globstart sub.L1, 1
g_push 0
g_eval
g_proj 2
g_update 2
g_pop 1
g_unwind

g_globstart foldr.L1, 1
g_push 0
g_eval
g_proj 0
g_update 2
g_pop 1
g_unwind

g_globstart foldl.L1, 1
g_push 0
g_eval
g_proj 1
g_update 2
g_pop 1
g_unwind

g_globstart monoidList.L1, 0
g_pushglobal monoidList.append.L1
g_pushglobal monoidList.empty.L1
g_updcons 0, 2, 1
g_return

g_globstart monoidList.empty.L1, 0
g_pushglobal C.0.0
g_update 1
g_unwind

g_globstart monoidList.append.L1, 2
g_push 0
g_push 2
g_pushglobal C.1.2
g_pushglobal foldableList
g_pushglobal foldr.L1
g_updap 4, 3
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
g_pushglobal foldableList
g_pushglobal foldr.L1
g_mkap 4
g_push 1
g_push 4
g_updap 2, 6
g_pop 5
g_unwind
g_label .2

g_globstart foldableList.foldl.L1, 3
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
g_pushglobal foldableList
g_pushglobal foldl.L1
g_updap 4, 6
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
g_pushglobal ordInt
g_pushglobal le.L1
g_mkap 3
g_eval
g_jumpcase .3, .4
g_label .3
g_pop 1
g_pushint 1
g_push 4
g_pushglobal ringInt
g_pushglobal sub.L1
g_mkap 3
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

g_globstart bind.L1, 1
g_push 0
g_eval
g_proj 1
g_update 2
g_pop 1
g_unwind

g_globstart monadIO.pure.L1, 0
g_pushglobal C.0.2
g_update 1
g_unwind

g_globstart monadIO.pure.L2, 1
g_push 0
g_pushglobal monadIO.pure.L1
g_updap 1, 2
g_pop 1
g_unwind

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

g_globstart monadIO.bind.L2, 2
g_push 1
g_push 1
g_pushglobal monadIO.bind.L1
g_updap 2, 3
g_pop 2
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

g_globstart print.L1, 0
g_pushglobal B.puti
g_pushglobal io.L2
g_updap 1, 1
g_unwind

g_globstart repeat.L1, 1
g_alloc 1
g_push 0
g_push 2
g_pushglobal monoidList.L1
g_pushglobal append.L1
g_updap 3, 1
g_update 2
g_pop 1
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
g_pushglobal add.L1
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

g_globstart filter.L2, 1
g_alloc 1
g_push 0
g_push 2
g_pushglobal filter.L1
g_updap 2, 1
g_update 2
g_pop 1
g_unwind

g_globstart sieve.L1, 2
g_pushint 0
g_push 1
g_push 3
g_pushglobal B.mod
g_mkap 2
g_pushglobal eqInt
g_pushglobal neq.L1
g_updap 3, 3
g_pop 2
g_unwind

g_globstart sieve.L2, 1
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
g_push 1
g_push 1
g_pushglobal sieve.L1
g_mkap 1
g_pushglobal filter.L2
g_mkap 2
g_pushglobal sieve.L2
g_mkap 1
g_push 1
g_updcons 1, 2, 4
g_pop 3
g_return
g_jump .2
g_label .2

g_globstart main.L1, 1
g_push 0
g_pushglobal primes
g_pushglobal nth_exn.L1
g_mkap 2
g_pushglobal print.L1
g_updap 1, 2
g_pop 1
g_unwind
