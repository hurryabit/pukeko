g_declare_cafs Nil, abort, psums, primes, main
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

g_globstart nth, 2
g_push 0
g_eval
g_jumpzero .0
g_uncons 2
g_pushint 0
g_push 4
g_eval
g_leq
g_jumpzero .2
g_uncons 0
g_push 0
g_eval
g_update 5
g_pop 4
g_unwind
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
g_update 5
g_pop 4
g_unwind
g_label .3
g_jump .1
g_label .0
g_uncons 0
g_pushglobal abort, 0
g_eval
g_update 3
g_pop 2
g_unwind
g_label .1

g_globstart append, 2
g_push 0
g_eval
g_jumpzero .0
g_uncons 2
g_push 3
g_push 2
g_pushglobal append, 2
g_mkap 2
g_push 1
g_cons 1, 2
g_update 5
g_pop 4
g_return
g_jump .1
g_label .0
g_uncons 0
g_push 1
g_eval
g_update 3
g_pop 2
g_unwind
g_label .1

g_globstart repeat, 1
g_alloc 1
g_push 0
g_push 2
g_pushglobal append, 2
g_mkap 2
g_update 1
g_push 0
g_eval
g_update 3
g_pop 2
g_unwind

g_globstart psums0, 2
g_push 1
g_eval
g_jumpzero .0
g_uncons 2
g_push 2
g_push 1
g_pushglobal prefix_add, 2
g_mkap 2
g_push 2
g_push 1
g_pushglobal psums0, 2
g_mkap 2
g_push 1
g_cons 1, 2
g_update 6
g_pop 5
g_return
g_jump .1
g_label .0
g_uncons 0
g_pushglobal Nil, 0
g_eval
g_update 3
g_pop 2
g_unwind
g_label .1

g_globstart psums, 0
g_pushint 0
g_pushglobal psums0, 2
g_mkap 1
g_update 1
g_pop 0
g_unwind

g_globstart filter_p$0, 3
g_push 2
g_eval
g_jumpzero .0
g_uncons 2
g_push 1
g_push 3
g_mkap 1
g_push 1
g_push 5
g_mkap 1
g_eval
g_jumpzero .2
g_uncons 0
g_push 0
g_push 2
g_cons 1, 2
g_update 7
g_pop 6
g_return
g_jump .3
g_label .2
g_uncons 0
g_push 0
g_eval
g_update 7
g_pop 6
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

g_globstart filter, 1
g_alloc 1
g_push 1
g_push 1
g_pushglobal filter_p$0, 3
g_mkap 2
g_update 1
g_push 0
g_eval
g_update 3
g_pop 2
g_unwind

g_globstart sieve$1, 2
g_pushint 0
g_push 1
g_eval
g_push 3
g_eval
g_mod
g_neq
g_update 3
g_pop 2
g_return

g_globstart sieve, 1
g_push 0
g_eval
g_jumpzero .0
g_uncons 2
g_push 1
g_push 1
g_pushglobal sieve$1, 2
g_mkap 1
g_pushglobal filter, 1
g_mkap 2
g_pushglobal sieve, 1
g_mkap 1
g_push 1
g_cons 1, 2
g_update 4
g_pop 3
g_return
g_jump .1
g_label .0
g_uncons 0
g_pushglobal abort, 0
g_eval
g_update 2
g_pop 1
g_unwind
g_label .1

g_globstart primes, 0
g_pushglobal Nil, 0
g_pushint 4
g_cons 1, 2
g_pushint 2
g_cons 1, 2
g_pushglobal repeat, 1
g_mkap 1
g_pushint 5
g_cons 1, 2
g_pushglobal psums, 0
g_mkap 1
g_pushglobal sieve, 1
g_mkap 1
g_pushint 3
g_cons 1, 2
g_pushint 2
g_cons 1, 2
g_update 1
g_pop 0
g_return

g_globstart main, 0
g_pushint 5999
g_pushglobal primes, 0
g_pushglobal nth, 2
g_mkap 2
g_pushglobal print, 2
g_mkap 1
g_update 1
g_pop 0
g_unwind