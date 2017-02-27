g_declare_cafs Nil, abort, prime, fibs0, fibs1, main
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

g_globstart prefix_mul, 2
g_push 1
g_eval
g_push 1
g_eval
g_mul
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

g_globstart Nil, 0
g_cons 0, 0
g_update 1
g_return

g_globstart Cons, 2
g_cons 1, 2
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

g_globstart nth, 2
g_push 0
g_eval
g_jumpzero .0
g_uncons 2
g_pushint 0
g_push 4
g_pushglobal prefix_le, 2
g_mkap 2
g_eval
g_jumpzero .2
g_uncons 0
g_push 0
g_slide 0
g_jump .3
g_label .2
g_uncons 0
g_pushint 1
g_push 4
g_pushglobal prefix_sub, 2
g_mkap 2
g_push 2
g_pushglobal nth, 2
g_mkap 2
g_slide 0
g_label .3
g_slide 2
g_jump .1
g_label .0
g_uncons 0
g_pushglobal abort, 0
g_slide 0
g_label .1
g_update 3
g_pop 2
g_unwind

g_globstart zip_with, 3
g_push 1
g_eval
g_jumpzero .0
g_uncons 2
g_push 4
g_eval
g_jumpzero .2
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
g_pushglobal Cons, 2
g_mkap 2
g_slide 2
g_jump .3
g_label .2
g_uncons 0
g_pushglobal Nil, 0
g_slide 0
g_label .3
g_slide 2
g_jump .1
g_label .0
g_uncons 0
g_pushglobal Nil, 0
g_slide 0
g_label .1
g_update 4
g_pop 3
g_unwind

g_globstart prime, 0
g_pushint 39
g_pushint 1000000
g_pushint 1000000
g_pushglobal prefix_mul, 2
g_mkap 2
g_pushglobal prefix_add, 2
g_mkap 2
g_update 1
g_pop 0
g_unwind

g_globstart add_mod_prime, 2
g_push 1
g_push 1
g_pushglobal prefix_add, 2
g_mkap 2
g_pushglobal prime, 0
g_push 1
g_pushglobal prefix_lt, 2
g_mkap 2
g_eval
g_jumpzero .0
g_uncons 0
g_push 0
g_slide 0
g_jump .1
g_label .0
g_uncons 0
g_pushglobal prime, 0
g_push 1
g_pushglobal prefix_sub, 2
g_mkap 2
g_slide 0
g_label .1
g_slide 1
g_update 3
g_pop 2
g_unwind

g_globstart fibs0, 0
g_pushglobal fibs1, 0
g_pushint 0
g_pushglobal Cons, 2
g_mkap 2
g_update 1
g_pop 0
g_unwind

g_globstart fibs1, 0
g_pushglobal fibs1, 0
g_pushglobal fibs0, 0
g_pushglobal add_mod_prime, 2
g_pushglobal zip_with, 3
g_mkap 3
g_pushint 1
g_pushglobal Cons, 2
g_mkap 2
g_update 1
g_pop 0
g_unwind

g_globstart main, 0
g_pushint 1500000
g_pushglobal fibs0, 0
g_pushglobal nth, 2
g_mkap 2
g_pushglobal print, 2
g_mkap 1
g_update 1
g_pop 0
g_unwind
