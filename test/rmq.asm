g_declare_globals C.0.0, 0, C.0.2, 2, C.0.4, 4, C.1.0, 0, C.1.2, 2, C.1.5, 5, B.abort, 0, B.lt, 2, B.le, 2, B.ge, 2, B.gt, 2, B.neg, 1, B.add, 2, B.sub, 2, B.mul, 2, B.seq, 2, B.puti, 1, B.geti, 1, ordInt, 0, ringInt, 0, monadIO, 0, print, 0, input, 0, nats, 0, infinity, 0, main, 0, replicate.L1, 2, zip_with.L1, 3, sequence.L1, 3, sequence.L2, 3, sequence.L3, 2, monadIO.pure.L2, 1, monadIO.bind.L1, 3, monadIO.bind.L2, 2, io.L1, 3, io.L2, 2, nats.L1, 2, pair.L1, 2, single.L1, 2, combine.L1, 3, build.L1, 3, query.L1, 6, min.L1, 2, main.L1, 3, main.L2, 2, main.L3, 1, main.L4, 2, main.L5, 2, main.L6, 1
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

g_globstart C.1.5, 5
g_updcons 1, 5, 1
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
g_pushglobal io.L1
g_updap 2, 1
g_unwind

g_globstart nats, 0
g_alloc 1
g_push 0
g_pushglobal nats.L1
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
g_pushglobal main.L6
g_pushglobal input
g_pushglobal monadIO
g_eval
g_proj 1
g_push 0
g_slide 1
g_updap 2, 1
g_unwind

g_globstart replicate.L1, 2
g_pushint 0
g_push 1
g_pushglobal ordInt
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
g_pushglobal ringInt
g_eval
g_proj 2
g_push 0
g_slide 1
g_mkap 2
g_pushglobal replicate.L1
g_mkap 2
g_push 2
g_updcons 1, 2, 3
g_pop 2
g_return
g_jump .2
g_label .1
g_pop 1
g_pushglobal C.0.0
g_update 3
g_pop 2
g_unwind
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

g_globstart sequence.L1, 3
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

g_globstart sequence.L2, 3
g_push 2
g_push 1
g_pushglobal sequence.L1
g_mkap 2
g_push 2
g_push 2
g_pushglobal sequence.L3
g_mkap 2
g_push 2
g_eval
g_proj 1
g_push 0
g_slide 1
g_updap 2, 4
g_pop 3
g_unwind

g_globstart sequence.L3, 2
g_push 1
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal C.0.0
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
g_pushglobal sequence.L2
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

g_globstart monadIO.pure.L2, 1
g_push 0
g_pushglobal C.0.2
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

g_globstart nats.L1, 2
g_pushint 1
g_push 2
g_pushglobal ringInt
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

g_globstart pair.L1, 2
g_push 1
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal C.0.0
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
g_pushglobal pair.L1
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

g_globstart single.L1, 2
g_pushglobal C.0.0
g_pushglobal C.0.0
g_push 3
g_push 3
g_push 4
g_updcons 1, 5, 3
g_pop 2
g_return

g_globstart combine.L1, 3
g_push 1
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal B.abort
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
g_pushglobal B.abort
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

g_globstart build.L1, 3
g_push 2
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal B.abort
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
g_pushglobal combine.L1
g_mkap 1
g_pushglobal pair.L1
g_mkap 2
g_push 6
g_updap 1, 8
g_pop 7
g_unwind
g_label .5
g_jump .2
g_label .2

g_globstart query.L1, 6
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
g_pushglobal ordInt
g_eval
g_proj 3
g_push 0
g_slide 1
g_mkap 2
g_push 2
g_push 9
g_pushglobal ordInt
g_eval
g_proj 1
g_push 0
g_slide 1
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
g_pushglobal C.1.0
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
g_pushglobal ordInt
g_eval
g_proj 2
g_push 0
g_slide 1
g_mkap 2
g_push 9
g_push 3
g_pushglobal ordInt
g_eval
g_proj 2
g_push 0
g_slide 1
g_mkap 2
g_push 1
g_eval
g_jumpcase .9, .10
g_label .9
g_pop 1
g_pushglobal C.0.0
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

g_globstart min.L1, 2
g_push 1
g_push 1
g_pushglobal ordInt
g_eval
g_proj 2
g_push 0
g_slide 1
g_mkap 2
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

g_globstart main.L1, 3
g_alloc 1
g_push 0
g_push 4
g_push 4
g_pushglobal min.L1
g_pushglobal infinity
g_pushglobal query.L1
g_updap 5, 1
g_push 1
g_push 1
g_mkap 1
g_slide 1
g_push 0
g_pushglobal print
g_updap 1, 5
g_pop 4
g_unwind

g_globstart main.L2, 2
g_push 1
g_push 1
g_pushglobal main.L1
g_mkap 2
g_pushglobal input
g_pushglobal monadIO
g_eval
g_proj 1
g_push 0
g_slide 1
g_updap 2, 3
g_pop 2
g_unwind

g_globstart main.L3, 1
g_pushglobal C.0.0
g_pushglobal monadIO
g_eval
g_proj 0
g_push 0
g_slide 1
g_updap 1, 2
g_pop 1
g_unwind

g_globstart main.L4, 2
g_alloc 1
g_push 0
g_pushglobal min.L1
g_pushglobal build.L1
g_updap 2, 1
g_push 2
g_pushglobal nats
g_pushglobal single.L1
g_pushglobal zip_with.L1
g_mkap 3
g_push 1
g_mkap 1
g_slide 1
g_pushglobal main.L3
g_push 1
g_pushglobal main.L2
g_mkap 1
g_pushglobal input
g_pushglobal monadIO
g_eval
g_proj 1
g_push 0
g_slide 1
g_mkap 2
g_push 0
g_push 4
g_pushglobal replicate.L1
g_mkap 2
g_pushglobal monadIO
g_pushglobal sequence.L3
g_mkap 2
g_slide 1
g_pushglobal monadIO
g_eval
g_proj 1
g_push 0
g_slide 1
g_updap 2, 4
g_pop 3
g_unwind

g_globstart main.L5, 2
g_push 1
g_pushglobal main.L4
g_mkap 1
g_pushglobal input
g_push 2
g_pushglobal replicate.L1
g_mkap 2
g_pushglobal monadIO
g_pushglobal sequence.L3
g_mkap 2
g_pushglobal monadIO
g_eval
g_proj 1
g_push 0
g_slide 1
g_updap 2, 3
g_pop 2
g_unwind

g_globstart main.L6, 1
g_push 0
g_pushglobal main.L5
g_mkap 1
g_pushglobal input
g_pushglobal monadIO
g_eval
g_proj 1
g_push 0
g_slide 1
g_updap 2, 2
g_pop 1
g_unwind
