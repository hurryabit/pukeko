g_declare_cafs dict$Monad$IO, main
g_declare_main main

g_globstart gm$cons_0_2, 2
g_updcons 0, 2, 1
g_return

g_globstart op$gge, 1
g_push 0
g_eval
g_uncons 2
g_pop 1
g_update 2
g_pop 1
g_unwind

g_globstart op$s$ll1, 2
g_update 2
g_pop 1
g_unwind

g_globstart op$s, 3
g_push 2
g_pushglobal op$s$ll1, 2
g_mkap 1
g_push 2
g_push 2
g_pushglobal op$gge, 1
g_mkap 1
g_updap 2, 4
g_pop 3
g_unwind

g_globstart gm$return, 2
g_updcons 0, 2, 1
g_return

g_globstart gm$bind, 3
g_push 2
g_push 1
g_mkap 1
g_eval
g_uncons 2
g_push 3
g_updap 2, 4
g_pop 3
g_unwind

g_globstart dict$Monad$IO, 0
g_pushglobal gm$return, 2
g_pushglobal gm$bind, 3
g_push 0
g_push 2
g_updcons 0, 2, 3
g_pop 2
g_return

g_globstart gm$print, 2
g_eval
g_print
g_cons 0, 0
g_updcons 0, 2, 1
g_return

g_globstart gm$input, 1
g_input
g_updcons 0, 2, 1
g_return

g_globstart fst, 1
g_push 0
g_eval
g_uncons 2
g_update 3
g_pop 2
g_unwind

g_globstart snd, 1
g_push 0
g_eval
g_uncons 2
g_pop 1
g_update 2
g_pop 1
g_unwind

g_globstart main$ll1, 2
g_push 1
g_push 1
g_cons 0, 2
g_push 0
g_pushglobal snd, 1
g_mkap 1
g_pushglobal gm$print, 2
g_mkap 1
g_push 1
g_pushglobal fst, 1
g_mkap 1
g_pushglobal gm$print, 2
g_mkap 1
g_pushglobal dict$Monad$IO, 0
g_pushglobal op$s, 3
g_mkap 1
g_updap 2, 4
g_pop 3
g_unwind

g_globstart main$ll2, 1
g_push 0
g_pushglobal main$ll1, 2
g_mkap 1
g_pushglobal gm$input, 1
g_pushglobal dict$Monad$IO, 0
g_pushglobal op$gge, 1
g_mkap 1
g_updap 2, 2
g_pop 1
g_unwind

g_globstart main, 0
g_pushglobal main$ll2, 1
g_pushglobal gm$input, 1
g_pushglobal dict$Monad$IO, 0
g_pushglobal op$gge, 1
g_mkap 1
g_updap 2, 1
g_unwind
