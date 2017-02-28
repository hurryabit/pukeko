g_declare_cafs Nil, prime, powers, sum, numbers, main
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

g_globstart foldr, 3
g_push 2
g_eval
g_jumpzero .0
g_uncons 2
g_push 1
g_push 4
g_push 4
g_pushglobal foldr, 3
g_mkap 3
g_push 1
g_push 4
g_mkap 2
g_update 6
g_pop 5
g_unwind
g_jump .1
g_label .0
g_uncons 0
g_push 1
g_eval
g_update 4
g_pop 3
g_unwind
g_label .1

g_globstart take, 2
g_pushint 0
g_push 1
g_eval
g_leq
g_jumpzero .0
g_uncons 0
g_pushglobal Nil, 0
g_eval
g_update 3
g_pop 2
g_unwind
g_jump .1
g_label .0
g_uncons 0
g_push 1
g_eval
g_jumpzero .2
g_uncons 2
g_push 1
g_pushint 1
g_push 4
g_pushglobal prefix_sub, 2
g_mkap 2
g_pushglobal take, 2
g_mkap 2
g_push 1
g_cons 1, 2
g_update 5
g_pop 4
g_return
g_jump .3
g_label .2
g_uncons 0
g_pushglobal Nil, 0
g_eval
g_update 3
g_pop 2
g_unwind
g_label .3
g_label .1

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
g_cons 1, 2
g_update 8
g_pop 7
g_return
g_jump .3
g_label .2
g_uncons 0
g_pushglobal Nil, 0
g_eval
g_update 6
g_pop 5
g_unwind
g_label .3
g_jump .1
g_label .0
g_uncons 0
g_pushglobal Nil, 0
g_eval
g_update 4
g_pop 3
g_unwind
g_label .1

g_globstart prime, 0
g_pushint 39
g_pushint 1000000
g_pushint 1000000
g_mul
g_add
g_update 1
g_pop 0
g_return

g_globstart double_mod_prime, 1
g_push 0
g_pushint 2
g_pushglobal prefix_mul, 2
g_mkap 2
g_pushglobal prime, 0
g_eval
g_push 1
g_eval
g_les
g_jumpzero .0
g_uncons 0
g_push 0
g_eval
g_update 3
g_pop 2
g_unwind
g_jump .1
g_label .0
g_uncons 0
g_pushglobal prime, 0
g_eval
g_push 1
g_eval
g_sub
g_update 3
g_pop 2
g_return
g_label .1

g_globstart gen, 2
g_push 1
g_push 1
g_mkap 1
g_push 1
g_pushglobal gen, 2
g_mkap 2
g_push 2
g_cons 1, 2
g_update 3
g_pop 2
g_return

g_globstart powers, 0
g_pushint 1
g_pushglobal double_mod_prime, 1
g_pushglobal gen, 2
g_mkap 2
g_update 1
g_pop 0
g_unwind

g_globstart sum, 0
g_pushint 0
g_pushglobal prefix_add, 2
g_pushglobal foldr, 3
g_mkap 2
g_update 1
g_pop 0
g_unwind

g_globstart mul_mod_prime, 2
g_pushglobal prime, 0
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

g_globstart sum_prod, 2
g_push 1
g_push 1
g_pushglobal mul_mod_prime, 2
g_pushglobal zip_with, 3
g_mkap 3
g_pushglobal sum, 0
g_mkap 1
g_update 3
g_pop 2
g_unwind

g_globstart hash, 1
g_pushglobal prime, 0
g_eval
g_pushglobal powers, 0
g_push 2
g_pushglobal sum_prod, 2
g_mkap 2
g_eval
g_mod
g_update 2
g_pop 1
g_return

g_globstart numbers$1, 1
g_pushint 10007
g_push 1
g_eval
g_pushint 42
g_mul
g_mod
g_update 2
g_pop 1
g_return

g_globstart numbers, 0
g_pushint 1
g_pushglobal numbers$1, 1
g_pushglobal gen, 2
g_mkap 2
g_pushint 10006
g_pushglobal take, 2
g_mkap 2
g_update 1
g_pop 0
g_unwind

g_globstart insert, 2
g_push 1
g_eval
g_jumpzero .0
g_uncons 2
g_push 0
g_eval
g_push 3
g_eval
g_leq
g_jumpzero .2
g_uncons 0
g_push 3
g_push 3
g_cons 1, 2
g_update 5
g_pop 4
g_return
g_jump .3
g_label .2
g_uncons 0
g_push 1
g_push 3
g_pushglobal insert, 2
g_mkap 2
g_push 1
g_cons 1, 2
g_update 5
g_pop 4
g_return
g_label .3
g_jump .1
g_label .0
g_uncons 0
g_pushglobal Nil, 0
g_push 1
g_cons 1, 2
g_update 3
g_pop 2
g_return
g_label .1

g_globstart isort, 1
g_push 0
g_eval
g_jumpzero .0
g_uncons 2
g_push 1
g_pushglobal isort, 1
g_mkap 1
g_push 1
g_pushglobal insert, 2
g_mkap 2
g_update 4
g_pop 3
g_unwind
g_jump .1
g_label .0
g_uncons 0
g_pushglobal Nil, 0
g_eval
g_update 2
g_pop 1
g_unwind
g_label .1

g_globstart main, 0
g_pushglobal numbers, 0
g_pushglobal isort, 1
g_mkap 1
g_pushglobal hash, 1
g_mkap 1
g_pushglobal print, 2
g_mkap 1
g_update 1
g_pop 0
g_unwind
