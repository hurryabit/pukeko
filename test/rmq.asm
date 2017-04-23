g_declare_cafs False, Nil, RmqEmpty, True, Unit, abort, nats, infinity, main
g_declare_main main

g_globstart prefix_add, 2
g_push 1
g_eval
g_push 1
g_eval
g_add
g_update 3
g_pop 2
g_return

g_globstart prefix_sub, 2
g_push 1
g_eval
g_push 1
g_eval
g_sub
g_update 3
g_pop 2
g_return

g_globstart prefix_lt, 2
g_push 1
g_eval
g_push 1
g_eval
g_les
g_update 3
g_pop 2
g_return

g_globstart prefix_le, 2
g_push 1
g_eval
g_push 1
g_eval
g_leq
g_update 3
g_pop 2
g_return

g_globstart prefix_gt, 2
g_push 1
g_eval
g_push 1
g_eval
g_gtr
g_update 3
g_pop 2
g_return

g_globstart False, 0
g_updcons 0, 0, 1
g_return

g_globstart Nil, 0
g_updcons 0, 0, 1
g_return

g_globstart RmqEmpty, 0
g_updcons 0, 0, 1
g_return

g_globstart True, 0
g_updcons 1, 0, 1
g_return

g_globstart Unit, 0
g_updcons 0, 0, 1
g_return

g_globstart return, 2
g_updcons 0, 2, 1
g_return

g_globstart print, 2
g_eval
g_print
g_cons 0, 0
g_updcons 0, 2, 1
g_return

g_globstart input, 1
g_input
g_updcons 0, 2, 1
g_return

g_globstart prefix_bind, 3
g_push 2
g_push 1
g_mkap 1
g_eval
g_uncons 2
g_push 3
g_updap 2, 4
g_pop 3
g_unwind

g_globstart abort, 0
g_abort

g_globstart prefix_and, 2
g_push 0
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal False, 0
g_update 3
g_pop 2
g_unwind
g_label .1
g_pop 2
g_update 1
g_unwind
g_label .2

g_globstart prefix_or, 2
g_push 0
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 2
g_update 1
g_unwind
g_label .1
g_pop 1
g_pushglobal True, 0
g_update 3
g_pop 2
g_unwind
g_label .2

g_globstart zip_with, 3
g_push 1
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal Nil, 0
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
g_pushglobal Nil, 0
g_update 6
g_pop 5
g_unwind
g_label .4
g_uncons 2
g_push 1
g_push 4
g_push 6
g_pushglobal zip_with, 3
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

g_globstart replicate, 2
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
g_pushglobal prefix_sub, 2
g_mkap 2
g_pushglobal replicate, 2
g_mkap 2
g_push 2
g_updcons 1, 2, 3
g_pop 2
g_return
g_jump .2
g_label .1
g_pop 1
g_pushglobal Nil, 0
g_update 3
g_pop 2
g_unwind
g_label .2

g_globstart sequence_io$2, 2
g_push 1
g_push 1
g_cons 1, 2
g_pushglobal return, 2
g_updap 1, 3
g_pop 2
g_unwind

g_globstart sequence_io$1, 2
g_push 1
g_pushglobal sequence_io$2, 2
g_mkap 1
g_push 1
g_pushglobal sequence_io, 1
g_mkap 1
g_pushglobal prefix_bind, 3
g_updap 2, 3
g_pop 2
g_unwind

g_globstart sequence_io, 1
g_push 0
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal Nil, 0
g_pushglobal return, 2
g_updap 1, 2
g_pop 1
g_unwind
g_label .1
g_uncons 2
g_push 1
g_pushglobal sequence_io$1, 2
g_mkap 1
g_push 1
g_pushglobal prefix_bind, 3
g_updap 2, 4
g_pop 3
g_unwind
g_label .2

g_globstart nats$1, 2
g_pushint 1
g_push 2
g_pushglobal prefix_add, 2
g_mkap 2
g_push 1
g_mkap 1
g_push 2
g_updcons 1, 2, 3
g_pop 2
g_return

g_globstart nats, 0
g_alloc 1
g_push 0
g_pushglobal nats$1, 2
g_updap 1, 1
g_pushint 0
g_push 1
g_updap 1, 2
g_pop 1
g_unwind

g_globstart pair, 2
g_push 1
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal Nil, 0
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
g_pushglobal pair, 2
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

g_globstart single, 2
g_pushglobal RmqEmpty, 0
g_pushglobal RmqEmpty, 0
g_push 3
g_push 3
g_push 4
g_updcons 1, 5, 3
g_pop 2
g_return

g_globstart combine, 3
g_push 1
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal abort, 0
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
g_pushglobal abort, 0
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

g_globstart build$1, 3
g_push 2
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal abort, 0
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
g_pushglobal combine, 3
g_mkap 1
g_pushglobal pair, 2
g_mkap 2
g_push 6
g_updap 1, 8
g_pop 7
g_unwind
g_label .5
g_jump .2
g_label .2

g_globstart build, 2
g_alloc 1
g_push 0
g_push 2
g_pushglobal build$1, 3
g_updap 2, 1
g_push 2
g_pushglobal nats, 0
g_pushglobal single, 2
g_pushglobal zip_with, 3
g_mkap 3
g_push 1
g_updap 1, 4
g_pop 3
g_unwind

g_globstart query$1, 6
g_push 5
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 2
g_update 5
g_pop 4
g_unwind
g_label .1
g_uncons 5
g_push 1
g_push 10
g_pushglobal prefix_gt, 2
g_mkap 2
g_push 1
g_push 10
g_pushglobal prefix_lt, 2
g_mkap 2
g_pushglobal prefix_or, 2
g_mkap 2
g_eval
g_jumpcase .3, .4
g_label .3
g_pop 1
g_push 8
g_push 2
g_pushglobal prefix_le, 2
g_mkap 2
g_push 1
g_push 11
g_pushglobal prefix_le, 2
g_mkap 2
g_pushglobal prefix_and, 2
g_mkap 2
g_eval
g_jumpcase .6, .7
g_label .6
g_pop 1
g_push 4
g_push 6
g_mkap 1
g_push 4
g_push 7
g_mkap 1
g_push 9
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
g_pop 7
g_update 5
g_pop 4
g_unwind
g_label .5
g_jump .2
g_label .2

g_globstart query, 4
g_alloc 1
g_push 3
g_push 5
g_push 4
g_push 4
g_push 4
g_pushglobal query$1, 6
g_updap 5, 1
g_update 5
g_pop 4
g_unwind

g_globstart infinity, 0
g_pushint 1000000000
g_update 1
g_return

g_globstart min, 2
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

g_globstart replicate_io, 2
g_push 1
g_push 1
g_pushglobal replicate, 2
g_mkap 2
g_pushglobal sequence_io, 1
g_updap 1, 3
g_pop 2
g_unwind

g_globstart main$5, 3
g_push 1
g_push 3
g_push 2
g_pushglobal min, 2
g_pushglobal infinity, 0
g_pushglobal query, 4
g_mkap 5
g_push 0
g_pushglobal print, 2
g_updap 1, 5
g_pop 4
g_unwind

g_globstart main$4, 2
g_push 0
g_push 2
g_pushglobal main$5, 3
g_mkap 2
g_pushglobal input, 1
g_pushglobal prefix_bind, 3
g_updap 2, 3
g_pop 2
g_unwind

g_globstart main$6, 1
g_pushglobal Unit, 0
g_pushglobal return, 2
g_updap 1, 2
g_pop 1
g_unwind

g_globstart main$3, 2
g_push 1
g_pushglobal min, 2
g_pushglobal build, 2
g_mkap 2
g_pushglobal main$6, 1
g_push 1
g_pushglobal main$4, 2
g_mkap 1
g_pushglobal input, 1
g_pushglobal prefix_bind, 3
g_mkap 2
g_push 3
g_pushglobal replicate_io, 2
g_mkap 2
g_pushglobal prefix_bind, 3
g_updap 2, 4
g_pop 3
g_unwind

g_globstart main$2, 2
g_push 1
g_pushglobal main$3, 2
g_mkap 1
g_pushglobal input, 1
g_push 2
g_pushglobal replicate_io, 2
g_mkap 2
g_pushglobal prefix_bind, 3
g_updap 2, 3
g_pop 2
g_unwind

g_globstart main$1, 1
g_push 0
g_pushglobal main$2, 2
g_mkap 1
g_pushglobal input, 1
g_pushglobal prefix_bind, 3
g_updap 2, 2
g_pop 1
g_unwind

g_globstart main, 0
g_pushglobal main$1, 1
g_pushglobal input, 1
g_pushglobal prefix_bind, 3
g_updap 2, 1
g_unwind
