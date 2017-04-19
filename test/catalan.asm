g_declare_cafs Nil, abort, p, sum_p, sols, main
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

g_globstart prefix_mod, 2
g_push 1
g_eval
g_push 1
g_eval
g_mod
g_update 3
g_pop 2
g_return

g_globstart Nil, 0
g_cons 0, 0
g_update 1
g_return

g_globstart print, 2
g_eval
g_print
g_cons 0, 0
g_cons 0, 2
g_update 1
g_return

g_globstart abort, 0
g_abort

g_globstart foldl, 3
g_push 2
g_eval
g_jumpcase .0, .1
g_label .0
g_uncons 0
g_push 1
g_eval
g_update 4
g_pop 3
g_unwind
g_jump .2
g_label .1
g_uncons 2
g_push 1
g_push 1
g_push 5
g_push 5
g_mkap 2
g_push 4
g_pushglobal foldl, 3
g_mkap 3
g_update 6
g_pop 5
g_unwind
g_jump .2
g_label .2

g_globstart nth, 2
g_push 0
g_eval
g_jumpcase .0, .1
g_label .0
g_uncons 0
g_pushglobal abort, 0
g_eval
g_update 3
g_pop 2
g_unwind
g_jump .2
g_label .1
g_uncons 2
g_pushint 0
g_push 4
g_eval
g_leq
g_jumpcase .3, .4
g_label .3
g_uncons 0
g_pushint 1
g_push 4
g_pushglobal prefix_sub, 2
g_mkap 2
g_push 2
g_pushglobal nth, 2
g_mkap 2
g_update 5
g_pop 4
g_unwind
g_jump .5
g_label .4
g_uncons 0
g_push 0
g_eval
g_update 5
g_pop 4
g_unwind
g_jump .5
g_label .5
g_jump .2
g_label .2

g_globstart zip_with, 3
g_push 1
g_eval
g_jumpcase .0, .1
g_label .0
g_uncons 0
g_pushglobal Nil, 0
g_eval
g_update 4
g_pop 3
g_unwind
g_jump .2
g_label .1
g_uncons 2
g_push 4
g_eval
g_jumpcase .3, .4
g_label .3
g_uncons 0
g_pushglobal Nil, 0
g_eval
g_update 6
g_pop 5
g_unwind
g_jump .5
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
g_cons 1, 2
g_update 8
g_pop 7
g_return
g_jump .5
g_label .5
g_jump .2
g_label .2

g_globstart map, 2
g_push 1
g_eval
g_jumpcase .0, .1
g_label .0
g_uncons 0
g_pushglobal Nil, 0
g_eval
g_update 3
g_pop 2
g_unwind
g_jump .2
g_label .1
g_uncons 2
g_push 1
g_push 3
g_pushglobal map, 2
g_mkap 2
g_push 1
g_push 4
g_mkap 1
g_cons 1, 2
g_update 5
g_pop 4
g_return
g_jump .2
g_label .2

g_globstart p, 0
g_pushint 100000007
g_update 1
g_pop 0
g_return

g_globstart mul_p, 2
g_pushglobal p, 0
g_eval
g_push 2
g_eval
g_push 2
g_eval
g_mul
g_mod
g_update 3
g_pop 2
g_return

g_globstart add_p, 2
g_pushglobal p, 0
g_eval
g_push 2
g_eval
g_push 2
g_eval
g_add
g_mod
g_update 3
g_pop 2
g_return

g_globstart sum_p, 0
g_pushint 0
g_pushglobal add_p, 2
g_pushglobal foldl, 3
g_mkap 2
g_update 1
g_pop 0
g_unwind

g_globstart scanl$1, 4
g_push 3
g_eval
g_jumpcase .0, .1
g_label .0
g_uncons 0
g_pushglobal Nil, 0
g_eval
g_update 5
g_pop 4
g_unwind
g_jump .2
g_label .1
g_uncons 2
g_push 0
g_push 5
g_push 4
g_mkap 2
g_push 2
g_push 1
g_push 6
g_mkap 2
g_push 1
g_cons 1, 2
g_update 8
g_pop 7
g_return
g_jump .2
g_label .2

g_globstart scanl, 1
g_alloc 1
g_push 0
g_push 2
g_pushglobal scanl$1, 4
g_mkap 2
g_update 1
g_push 0
g_eval
g_update 3
g_pop 2
g_unwind

g_globstart sols$1, 1
g_push 0
g_pushglobal sols, 0
g_pushglobal mul_p, 2
g_pushglobal zip_with, 3
g_mkap 3
g_pushglobal sum_p, 0
g_mkap 1
g_update 2
g_pop 1
g_unwind

g_globstart sols$2, 2
g_push 0
g_push 2
g_cons 1, 2
g_update 3
g_pop 2
g_return

g_globstart sols, 0
g_pushglobal sols, 0
g_pushglobal Nil, 0
g_pushglobal sols$2, 2
g_mkap 0
g_pushglobal scanl, 1
g_mkap 3
g_pushglobal sols$1, 1
g_mkap 0
g_pushglobal map, 2
g_mkap 2
g_pushint 1
g_cons 1, 2
g_update 1
g_pop 0
g_return

g_globstart main, 0
g_pushglobal p, 0
g_pushint 2000
g_pushglobal sols, 0
g_pushglobal nth, 2
g_mkap 2
g_pushglobal prefix_mod, 2
g_mkap 2
g_pushglobal print, 2
g_mkap 1
g_update 1
g_pop 0
g_unwind
