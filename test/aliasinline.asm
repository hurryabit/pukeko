g_declare_cafs gm$cons_0_0, dict$Monad$IO, main
g_declare_main main

g_globstart gm$cons_0_0, 0
g_updcons 0, 0, 1
g_return

g_globstart gm$cons_0_2, 2
g_updcons 0, 2, 1
g_return

g_globstart pure, 1
g_push 0
g_eval
g_uncons 2
g_update 3
g_pop 2
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

g_globstart h, 1
g_update 1
g_unwind

g_globstart main, 0
g_pushglobal gm$cons_0_0, 0
g_pushglobal h, 1
g_mkap 1
g_pushglobal h, 1
g_mkap 1
g_pushglobal dict$Monad$IO, 0
g_pushglobal pure, 1
g_mkap 1
g_updap 1, 1
g_unwind
