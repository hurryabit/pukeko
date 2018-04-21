g_declare_globals C.0.0, 0, C.0.2, 2, C.1.2, 2, B.eq, 2, B.lt, 2, B.le, 2, B.add, 2, B.sub, 2, B.seq, 2, B.puti, 1, B.geti, 1, monoidInt, 0, print, 0, input, 0, ints, 0, solve_aux, 1, main, 0, monoidInt.empty, 0, monoidList.empty, 0, foldMap.L1, 3, length.L1, 1, monoidList.append.L1, 2, functorList.map.L1, 2, foldableList.foldr.L1, 3, take.L1, 2, replicate.L1, 2, zip_of.L1, 3, monadIO.bind.L1, 3, io.L1, 3, io.L2, 2, diff.L1, 2, ints.L1, 2, solve_aux.L1, 3, solve_aux.L2, 2, main.L1, 1
g_declare_main main

g_globstart C.0.0, 0
g_updcons 0, 0, 1
g_return

g_globstart C.0.2, 2
g_updcons 0, 2, 1
g_return

g_globstart C.1.2, 2
g_updcons 1, 2, 1
g_return

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

g_globstart monoidInt, 0
g_pushglobal B.add
g_pushglobal monoidInt.empty
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

g_globstart ints, 0
g_alloc 1
g_push 0
g_pushglobal ints.L1
g_updap 1, 1
g_pushint 1
g_push 1
g_updap 1, 2
g_pop 1
g_unwind

g_globstart solve_aux, 1
g_push 0
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal C.0.0
g_pushglobal C.0.0
g_updcons 1, 2, 2
g_pop 1
g_return
g_jump .2
g_label .1
g_uncons 2
g_pushglobal monoidList.append.L1
g_pushglobal monoidList.empty
g_cons 0, 2
g_push 2
g_pushglobal solve_aux.L2
g_mkap 1
g_push 2
g_push 2
g_eval
g_proj 0
g_push 0
g_slide 1
g_push 2
g_push 4
g_pushglobal foldMap.L1
g_mkap 2
g_pushglobal foldableList.foldr.L1
g_updap 3, 6
g_pop 5
g_unwind
g_label .2

g_globstart main, 0
g_pushglobal main.L1
g_pushglobal input
g_pushglobal monadIO.bind.L1
g_updap 2, 1
g_unwind

g_globstart monoidInt.empty, 0
g_pushint 0
g_update 1
g_return

g_globstart monoidList.empty, 0
g_pushglobal C.0.0
g_update 1
g_unwind

g_globstart foldMap.L1, 3
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

g_globstart length.L1, 1
g_pushint 1
g_update 2
g_pop 1
g_return

g_globstart monoidList.append.L1, 2
g_push 0
g_push 2
g_pushglobal C.1.2
g_pushglobal foldableList.foldr.L1
g_updap 3, 3
g_pop 2
g_unwind

g_globstart functorList.map.L1, 2
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
g_push 3
g_pushglobal functorList.map.L1
g_mkap 2
g_push 1
g_push 4
g_mkap 1
g_updcons 1, 2, 5
g_pop 4
g_return
g_jump .2
g_label .2

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

g_globstart take.L1, 2
g_pushint 0
g_push 1
g_eval
g_leq
g_jumpcase .0, .1
g_label .0
g_pop 1
g_push 1
g_eval
g_jumpcase .3, .4
g_label .3
g_pop 1
g_pushglobal C.0.0
g_update 3
g_pop 2
g_unwind
g_label .4
g_uncons 2
g_push 1
g_pushint 1
g_push 4
g_pushglobal B.sub
g_mkap 2
g_pushglobal take.L1
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
g_pushglobal C.0.0
g_update 3
g_pop 2
g_unwind
g_label .2

g_globstart replicate.L1, 2
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
g_pushglobal B.sub
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

g_globstart zip_of.L1, 3
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
g_pushglobal zip_of.L1
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

g_globstart diff.L1, 2
g_push 0
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
g_eval
g_push 3
g_eval
g_les
g_jumpcase .6, .7
g_label .6
g_pop 1
g_push 0
g_eval
g_push 3
g_eval
g_eqv
g_jumpcase .9, .10
g_label .9
g_pop 1
g_push 1
g_push 5
g_pushglobal diff.L1
g_updap 2, 7
g_pop 6
g_unwind
g_label .10
g_pop 1
g_push 1
g_push 4
g_pushglobal diff.L1
g_updap 2, 7
g_pop 6
g_unwind
g_label .11
g_jump .8
g_label .7
g_pop 1
g_push 5
g_push 4
g_pushglobal diff.L1
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

g_globstart ints.L1, 2
g_pushint 1
g_push 2
g_pushglobal B.add
g_mkap 2
g_push 1
g_mkap 1
g_push 2
g_updcons 1, 2, 3
g_pop 2
g_return

g_globstart solve_aux.L1, 3
g_pushglobal C.0.0
g_push 3
g_push 2
g_pushglobal B.add
g_mkap 2
g_cons 1, 2
g_push 1
g_cons 1, 2
g_push 3
g_push 2
g_pushglobal B.sub
g_mkap 2
g_cons 1, 2
g_push 2
g_pushglobal diff.L1
g_updap 2, 4
g_pop 3
g_unwind

g_globstart solve_aux.L2, 2
g_pushglobal ints
g_push 1
g_push 3
g_pushglobal solve_aux.L1
g_mkap 1
g_pushglobal zip_of.L1
g_mkap 3
g_pushglobal solve_aux
g_mkap 1
g_push 2
g_pushglobal C.1.2
g_mkap 1
g_pushglobal functorList.map.L1
g_updap 2, 3
g_pop 2
g_unwind

g_globstart main.L1, 1
g_pushglobal length.L1
g_pushglobal ints
g_push 2
g_pushglobal take.L1
g_mkap 2
g_push 2
g_pushglobal replicate.L1
g_mkap 2
g_pushglobal solve_aux
g_mkap 1
g_pushglobal monoidInt.empty
g_push 2
g_pushglobal monoidInt
g_pushglobal foldMap.L1
g_mkap 2
g_pushglobal foldableList.foldr.L1
g_mkap 3
g_slide 1
g_pushglobal print
g_updap 1, 2
g_pop 1
g_unwind
