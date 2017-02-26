g_declare_cafs Nil, concat, length, ints, main
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

g_globstart prefix_eq, 2
g_push 1
g_eval
g_push 1
g_eval
g_eqv
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

g_globstart foldr, 3
g_push 2
g_eval
g_jumpzero .0
g_hdtl
g_push 1
g_push 4
g_push 4
g_pushglobal foldr, 3
g_mkap
g_mkap
g_mkap
g_push 1
g_push 4
g_mkap
g_mkap
g_slide 2
g_jump .1
g_label .0
g_pop 1
g_push 1
g_slide 0
g_label .1
g_update 4
g_pop 3
g_unwind

g_globstart take, 2
g_pushint 0
g_push 1
g_pushglobal prefix_le, 2
g_mkap
g_mkap
g_eval
g_jumpzero .0
g_pop 1
g_pushglobal Nil, 0
g_slide 0
g_jump .1
g_label .0
g_pop 1
g_push 1
g_eval
g_jumpzero .2
g_hdtl
g_push 1
g_pushint 1
g_push 4
g_pushglobal prefix_sub, 2
g_mkap
g_mkap
g_pushglobal take, 2
g_mkap
g_mkap
g_push 1
g_pushglobal Cons, 2
g_mkap
g_mkap
g_slide 2
g_jump .3
g_label .2
g_pop 1
g_pushglobal Nil, 0
g_slide 0
g_label .3
g_slide 0
g_label .1
g_update 3
g_pop 2
g_unwind

g_globstart zip_with, 3
g_push 1
g_eval
g_jumpzero .0
g_hdtl
g_push 4
g_eval
g_jumpzero .2
g_hdtl
g_push 1
g_push 4
g_push 6
g_pushglobal zip_with, 3
g_mkap
g_mkap
g_mkap
g_push 1
g_push 4
g_push 7
g_mkap
g_mkap
g_pushglobal Cons, 2
g_mkap
g_mkap
g_slide 2
g_jump .3
g_label .2
g_pop 1
g_pushglobal Nil, 0
g_slide 0
g_label .3
g_slide 2
g_jump .1
g_label .0
g_pop 1
g_pushglobal Nil, 0
g_slide 0
g_label .1
g_update 4
g_pop 3
g_unwind

g_globstart append, 2
g_push 0
g_eval
g_jumpzero .0
g_hdtl
g_push 3
g_push 2
g_pushglobal append, 2
g_mkap
g_mkap
g_push 1
g_pushglobal Cons, 2
g_mkap
g_mkap
g_slide 2
g_jump .1
g_label .0
g_pop 1
g_push 1
g_slide 0
g_label .1
g_update 3
g_pop 2
g_unwind

g_globstart concat, 0
g_pushglobal Nil, 0
g_pushglobal append, 2
g_pushglobal foldr, 3
g_mkap
g_mkap
g_update 1
g_pop 0
g_unwind

g_globstart map, 2
g_push 1
g_eval
g_jumpzero .0
g_hdtl
g_push 1
g_push 3
g_pushglobal map, 2
g_mkap
g_mkap
g_push 1
g_push 4
g_mkap
g_pushglobal Cons, 2
g_mkap
g_mkap
g_slide 2
g_jump .1
g_label .0
g_pop 1
g_pushglobal Nil, 0
g_slide 0
g_label .1
g_update 3
g_pop 2
g_unwind

g_globstart concat_map, 2
g_push 1
g_push 1
g_pushglobal map, 2
g_mkap
g_mkap
g_pushglobal concat, 0
g_mkap
g_update 3
g_pop 2
g_unwind

g_globstart length$1, 2
g_push 1
g_pushint 1
g_pushglobal prefix_add, 2
g_mkap
g_mkap
g_update 3
g_pop 2
g_unwind

g_globstart length, 0
g_pushint 0
g_pushglobal length$1, 2
g_pushglobal foldr, 3
g_mkap
g_mkap
g_update 1
g_pop 0
g_unwind

g_globstart replicate, 2
g_pushint 0
g_push 1
g_pushglobal prefix_le, 2
g_mkap
g_mkap
g_eval
g_jumpzero .0
g_pop 1
g_pushglobal Nil, 0
g_slide 0
g_jump .1
g_label .0
g_pop 1
g_push 1
g_pushint 1
g_push 2
g_pushglobal prefix_sub, 2
g_mkap
g_mkap
g_pushglobal replicate, 2
g_mkap
g_mkap
g_push 2
g_pushglobal Cons, 2
g_mkap
g_mkap
g_slide 0
g_label .1
g_update 3
g_pop 2
g_unwind

g_globstart diff, 2
g_push 0
g_eval
g_jumpzero .0
g_hdtl
g_push 3
g_eval
g_jumpzero .2
g_hdtl
g_push 0
g_push 3
g_pushglobal prefix_lt, 2
g_mkap
g_mkap
g_eval
g_jumpzero .4
g_pop 1
g_push 5
g_push 4
g_pushglobal diff, 2
g_mkap
g_mkap
g_push 3
g_pushglobal Cons, 2
g_mkap
g_mkap
g_slide 0
g_jump .5
g_label .4
g_pop 1
g_push 0
g_push 3
g_pushglobal prefix_eq, 2
g_mkap
g_mkap
g_eval
g_jumpzero .6
g_pop 1
g_push 1
g_push 4
g_pushglobal diff, 2
g_mkap
g_mkap
g_slide 0
g_jump .7
g_label .6
g_pop 1
g_push 1
g_push 5
g_pushglobal diff, 2
g_mkap
g_mkap
g_slide 0
g_label .7
g_slide 0
g_label .5
g_slide 2
g_jump .3
g_label .2
g_pop 1
g_push 2
g_slide 0
g_label .3
g_slide 2
g_jump .1
g_label .0
g_pop 1
g_pushglobal Nil, 0
g_slide 0
g_label .1
g_update 3
g_pop 2
g_unwind

g_globstart go, 1
g_pushint 1
g_push 1
g_pushglobal prefix_add, 2
g_mkap
g_mkap
g_pushglobal go, 1
g_mkap
g_push 1
g_pushglobal Cons, 2
g_mkap
g_mkap
g_update 2
g_pop 1
g_unwind

g_globstart ints, 0
g_pushint 1
g_pushglobal go, 1
g_mkap
g_update 1
g_pop 0
g_unwind

g_globstart solve$2, 3
g_pushglobal Nil, 0
g_push 3
g_push 2
g_pushglobal prefix_add, 2
g_mkap
g_mkap
g_pushglobal Cons, 2
g_mkap
g_mkap
g_push 1
g_pushglobal Cons, 2
g_mkap
g_mkap
g_push 3
g_push 2
g_pushglobal prefix_sub, 2
g_mkap
g_mkap
g_pushglobal Cons, 2
g_mkap
g_mkap
g_push 2
g_pushglobal diff, 2
g_mkap
g_mkap
g_update 4
g_pop 3
g_unwind

g_globstart solve$1, 2
g_pushglobal ints, 0
g_push 1
g_push 3
g_pushglobal solve$2, 3
g_mkap
g_pushglobal zip_with, 3
g_mkap
g_mkap
g_mkap
g_pushglobal solve, 1
g_mkap
g_push 2
g_pushglobal Cons, 2
g_mkap
g_pushglobal map, 2
g_mkap
g_mkap
g_update 3
g_pop 2
g_unwind

g_globstart solve, 1
g_push 0
g_eval
g_jumpzero .0
g_hdtl
g_push 0
g_push 2
g_pushglobal solve$1, 2
g_mkap
g_pushglobal concat_map, 2
g_mkap
g_mkap
g_slide 2
g_jump .1
g_label .0
g_pop 1
g_pushglobal Nil, 0
g_pushglobal Nil, 0
g_pushglobal Cons, 2
g_mkap
g_mkap
g_slide 0
g_label .1
g_update 2
g_pop 1
g_unwind

g_globstart solve$3, 1
g_pushglobal ints, 0
g_push 1
g_pushglobal take, 2
g_mkap
g_mkap
g_push 1
g_pushglobal replicate, 2
g_mkap
g_mkap
g_pushglobal solve, 1
g_mkap
g_update 2
g_pop 1
g_unwind

g_globstart main, 0
g_pushint 12
g_pushglobal solve$3, 1
g_mkap
g_pushglobal length, 0
g_mkap
g_pushglobal print, 2
g_mkap
g_update 1
g_pop 0
g_unwind
