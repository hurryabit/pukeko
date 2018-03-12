g_declare_globals C.0.0, 0, C.0.2, 2, C.0.4, 4, C.1.2, 2, B.abort, 0, B.lt, 2, B.le, 2, B.ge, 2, B.gt, 2, B.neg, 1, B.add, 2, B.sub, 2, B.mul, 2, B.seq, 2, B.puti, 1, B.geti, 1, le, 1, lt, 1, add, 1, sub, 1, mul, 1, ordInt, 0, ringInt, 0, bind, 1, monadIO, 0, print, 0, input, 0, prime, 0, fibs0, 0, fibs1, 0, main, 0, nth_exn.L1, 2, zip_with.L1, 3, monadIO.pure.L1, 0, monadIO.pure.L2, 1, monadIO.bind.L1, 3, monadIO.bind.L2, 2, io.L1, 3, io.L2, 2, add_mod_prime.L1, 2, main.L1, 1
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

g_globstart C.1.2, 2
g_updcons 1, 2, 1
g_return

g_globstart B.abort, 0
g_abort

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

g_globstart le, 1
g_push 0
g_eval
g_proj 2
g_update 2
g_pop 1
g_unwind

g_globstart lt, 1
g_push 0
g_eval
g_proj 3
g_update 2
g_pop 1
g_unwind

g_globstart add, 1
g_push 0
g_eval
g_proj 1
g_update 2
g_pop 1
g_unwind

g_globstart sub, 1
g_push 0
g_eval
g_proj 2
g_update 2
g_pop 1
g_unwind

g_globstart mul, 1
g_push 0
g_eval
g_proj 3
g_update 2
g_pop 1
g_unwind

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

g_globstart bind, 1
g_push 0
g_eval
g_proj 1
g_update 2
g_pop 1
g_unwind

g_globstart monadIO, 0
g_pushglobal monadIO.bind.L2
g_pushglobal monadIO.pure.L2
g_updcons 0, 2, 1
g_return

g_globstart print, 0
g_pushglobal B.puti
g_pushglobal io.L2
g_updap 1, 1
g_unwind

g_globstart input, 0
g_pushglobal C.0.0
g_pushglobal B.geti
g_pushglobal io.L2
g_updap 2, 1
g_unwind

g_globstart prime, 0
g_pushint 39
g_pushint 1000000
g_pushint 1000000
g_pushglobal ringInt
g_pushglobal mul
g_mkap 3
g_pushglobal ringInt
g_pushglobal add
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
g_pushglobal add_mod_prime.L1
g_pushglobal zip_with.L1
g_mkap 3
g_pushint 1
g_updcons 1, 2, 1
g_return

g_globstart main, 0
g_pushglobal main.L1
g_pushglobal input
g_pushglobal monadIO
g_pushglobal bind
g_updap 3, 1
g_unwind

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
g_pushglobal le
g_mkap 3
g_eval
g_jumpcase .3, .4
g_label .3
g_pop 1
g_pushint 1
g_push 4
g_pushglobal ringInt
g_pushglobal sub
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

g_globstart zip_with.L1, 3
g_push 1
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
g_push 4
g_eval
g_jumpcase .3, .4
g_label .3
g_pop 1
g_pushglobal C.0.0
g_update 6
g_pop 5
g_unwind
g_label .4
g_uncons 2
g_push 1
g_push 4
g_push 6
g_pushglobal zip_with.L1
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

g_globstart add_mod_prime.L1, 2
g_push 1
g_push 1
g_pushglobal ringInt
g_pushglobal add
g_mkap 3
g_pushglobal prime
g_push 1
g_pushglobal ordInt
g_pushglobal lt
g_mkap 3
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal prime
g_push 1
g_pushglobal ringInt
g_pushglobal sub
g_updap 3, 4
g_pop 3
g_unwind
g_label .1
g_pop 1
g_update 3
g_pop 2
g_unwind
g_label .2

g_globstart main.L1, 1
g_push 0
g_pushglobal fibs0
g_pushglobal nth_exn.L1
g_mkap 2
g_pushglobal print
g_updap 1, 2
g_pop 1
g_unwind
