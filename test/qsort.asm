g_declare_cafs Nil, Unit, main
g_declare_main main

g_globstart prefix_sub, 2
g_push 1
g_eval
g_push 1
g_eval
g_sub
g_update 3
g_pop 2
g_return

g_globstart Nil, 0
g_updcons 0, 0, 1
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

g_globstart foldr, 3
g_push 2
g_eval
g_jumpcase .0, .1
g_label .0
g_uncons 0
g_push 1
g_update 4
g_pop 3
g_unwind
g_label .1
g_uncons 2
g_push 1
g_push 4
g_push 4
g_pushglobal foldr, 3
g_mkap 3
g_push 1
g_push 4
g_updap 2, 6
g_pop 5
g_unwind
g_label .2

g_globstart partition$1, 3
g_push 2
g_eval
g_jumpcase .0, .1
g_label .0
g_uncons 0
g_pushglobal Nil, 0
g_pushglobal Nil, 0
g_updcons 0, 2, 4
g_pop 3
g_return
g_jump .2
g_label .1
g_uncons 2
g_push 1
g_push 4
g_mkap 1
g_eval
g_uncons 2
g_push 2
g_push 5
g_mkap 1
g_eval
g_jumpcase .3, .4
g_label .3
g_uncons 0
g_push 1
g_push 3
g_cons 1, 2
g_push 1
g_updcons 0, 2, 8
g_pop 7
g_return
g_jump .5
g_label .4
g_uncons 0
g_push 1
g_push 1
g_push 4
g_cons 1, 2
g_updcons 0, 2, 8
g_pop 7
g_return
g_jump .5
g_label .5
g_jump .2
g_label .2

g_globstart partition, 2
g_alloc 1
g_push 0
g_push 2
g_pushglobal partition$1, 3
g_updap 2, 1
g_push 2
g_push 1
g_updap 1, 4
g_pop 3
g_unwind

g_globstart append, 2
g_push 0
g_eval
g_jumpcase .0, .1
g_label .0
g_uncons 0
g_push 1
g_update 3
g_pop 2
g_unwind
g_label .1
g_uncons 2
g_push 3
g_push 2
g_pushglobal append, 2
g_mkap 2
g_push 1
g_updcons 1, 2, 5
g_pop 4
g_return
g_jump .2
g_label .2

g_globstart replicate, 2
g_pushint 0
g_push 1
g_eval
g_leq
g_jumpcase .0, .1
g_label .0
g_uncons 0
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
g_uncons 0
g_pushglobal Nil, 0
g_update 3
g_pop 2
g_unwind
g_label .2

g_globstart prefix_semi$1, 2
g_push 0
g_update 3
g_pop 2
g_unwind

g_globstart prefix_semi, 2
g_push 1
g_pushglobal prefix_semi$1, 2
g_mkap 1
g_push 1
g_pushglobal prefix_bind, 3
g_updap 2, 3
g_pop 2
g_unwind

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
g_uncons 0
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

g_globstart iter_io$1, 3
g_push 2
g_push 2
g_push 2
g_mkap 1
g_pushglobal prefix_semi, 2
g_updap 2, 4
g_pop 3
g_unwind

g_globstart iter_io, 1
g_pushglobal Unit, 0
g_pushglobal return, 2
g_mkap 1
g_push 1
g_pushglobal iter_io$1, 3
g_mkap 1
g_pushglobal foldr, 3
g_updap 2, 2
g_pop 1
g_unwind

g_globstart qsort$1, 2
g_push 0
g_eval
g_push 2
g_eval
g_les
g_update 3
g_pop 2
g_return

g_globstart qsort, 1
g_push 0
g_eval
g_jumpcase .0, .1
g_label .0
g_uncons 0
g_pushglobal Nil, 0
g_update 2
g_pop 1
g_unwind
g_label .1
g_uncons 2
g_push 1
g_push 1
g_pushglobal qsort$1, 2
g_mkap 1
g_pushglobal partition, 2
g_mkap 2
g_eval
g_uncons 2
g_push 1
g_pushglobal qsort, 1
g_mkap 1
g_push 3
g_cons 1, 2
g_push 1
g_pushglobal qsort, 1
g_mkap 1
g_pushglobal append, 2
g_updap 2, 6
g_pop 5
g_unwind
g_label .2

g_globstart main$2, 1
g_push 0
g_pushglobal qsort, 1
g_mkap 1
g_pushglobal print, 2
g_pushglobal iter_io, 1
g_updap 2, 2
g_pop 1
g_unwind

g_globstart main$1, 1
g_pushglobal main$2, 1
g_pushglobal input, 1
g_push 2
g_pushglobal replicate, 2
g_mkap 2
g_pushglobal sequence_io, 1
g_mkap 1
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
