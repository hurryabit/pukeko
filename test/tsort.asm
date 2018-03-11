g_declare_globals gm$cons_0_0, 0, gm$cons_0_2, 2, gm$cons_0_4, 4, gm$cons_1_2, 2, gm$cons_1_3, 3, gm$lt, 2, gm$le, 2, gm$ge, 2, gm$gt, 2, gm$sub, 2, gm$seq, 2, gm$puti, 1, gm$geti, 1, dict$Ord$Int, 0, dict$Foldable$List, 0, dict$Monad$IO, 0, input, 0, dict$Foldable$BinTree, 0, main, 0, dict$Foldable$List$ll1, 3, dict$Foldable$List$ll2, 3, replicate$ll1, 2, semi$ll1, 2, semi$ll2, 3, sequence$ll1, 3, sequence$ll2, 3, sequence$ll3, 2, traverse_$ll1, 3, dict$Monad$IO$ll2, 1, dict$Monad$IO$ll3, 3, dict$Monad$IO$ll4, 2, io$ll1, 3, io$ll2, 2, print$ll1, 0, dict$Foldable$BinTree$ll1, 3, dict$Foldable$BinTree$ll2, 3, bag_insert$ll1, 4, tsort$ll1, 2, main$ll1, 1, main$ll2, 1
g_declare_main main

g_globstart gm$cons_0_0, 0
g_updcons 0, 0, 1
g_return

g_globstart gm$cons_0_2, 2
g_updcons 0, 2, 1
g_return

g_globstart gm$cons_0_4, 4
g_updcons 0, 4, 1
g_return

g_globstart gm$cons_1_2, 2
g_updcons 1, 2, 1
g_return

g_globstart gm$cons_1_3, 3
g_updcons 1, 3, 1
g_return

g_globstart gm$lt, 2
g_push 1
g_eval
g_push 1
g_eval
g_les
g_update 3
g_pop 2
g_return

g_globstart gm$le, 2
g_push 1
g_eval
g_push 1
g_eval
g_leq
g_update 3
g_pop 2
g_return

g_globstart gm$ge, 2
g_push 1
g_eval
g_push 1
g_eval
g_geq
g_update 3
g_pop 2
g_return

g_globstart gm$gt, 2
g_push 1
g_eval
g_push 1
g_eval
g_gtr
g_update 3
g_pop 2
g_return

g_globstart gm$sub, 2
g_push 1
g_eval
g_push 1
g_eval
g_sub
g_update 3
g_pop 2
g_return

g_globstart gm$seq, 2
g_eval
g_pop 1
g_update 1
g_unwind

g_globstart gm$puti, 1
g_eval
g_print
g_updcons 0, 0, 1
g_return

g_globstart gm$geti, 1
g_pop 1
g_input
g_update 1
g_return

g_globstart dict$Ord$Int, 0
g_pushglobal gm$lt
g_pushglobal gm$le
g_pushglobal gm$gt
g_pushglobal gm$ge
g_updcons 0, 4, 1
g_return

g_globstart dict$Foldable$List, 0
g_pushglobal dict$Foldable$List$ll2
g_pushglobal dict$Foldable$List$ll1
g_updcons 0, 2, 1
g_return

g_globstart dict$Monad$IO, 0
g_pushglobal dict$Monad$IO$ll4
g_pushglobal dict$Monad$IO$ll2
g_updcons 0, 2, 1
g_return

g_globstart input, 0
g_pushglobal gm$cons_0_0
g_pushglobal gm$geti
g_pushglobal io$ll1
g_updap 2, 1
g_unwind

g_globstart dict$Foldable$BinTree, 0
g_pushglobal dict$Foldable$BinTree$ll2
g_pushglobal dict$Foldable$BinTree$ll1
g_updcons 0, 2, 1
g_return

g_globstart main, 0
g_pushglobal main$ll2
g_pushglobal input
g_pushglobal dict$Monad$IO$ll3
g_updap 2, 1
g_unwind

g_globstart dict$Foldable$List$ll1, 3
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
g_pushglobal dict$Foldable$List
g_eval
g_proj 0
g_push 0
g_slide 1
g_mkap 3
g_push 1
g_push 4
g_updap 2, 6
g_pop 5
g_unwind
g_label .2

g_globstart dict$Foldable$List$ll2, 3
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
g_push 1
g_push 5
g_push 5
g_mkap 2
g_push 4
g_pushglobal dict$Foldable$List
g_eval
g_proj 1
g_push 0
g_slide 1
g_updap 3, 6
g_pop 5
g_unwind
g_label .2

g_globstart replicate$ll1, 2
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
g_pushglobal gm$sub
g_mkap 2
g_pushglobal replicate$ll1
g_mkap 2
g_push 2
g_updcons 1, 2, 3
g_pop 2
g_return
g_jump .2
g_label .1
g_pop 1
g_pushglobal gm$cons_0_0
g_update 3
g_pop 2
g_unwind
g_label .2

g_globstart semi$ll1, 2
g_update 2
g_pop 1
g_unwind

g_globstart semi$ll2, 3
g_push 2
g_pushglobal semi$ll1
g_mkap 1
g_push 2
g_push 2
g_eval
g_proj 1
g_push 0
g_slide 1
g_updap 2, 4
g_pop 3
g_unwind

g_globstart sequence$ll1, 3
g_push 2
g_push 2
g_cons 1, 2
g_push 1
g_eval
g_proj 0
g_push 0
g_slide 1
g_updap 1, 4
g_pop 3
g_unwind

g_globstart sequence$ll2, 3
g_push 2
g_push 1
g_pushglobal sequence$ll1
g_mkap 2
g_push 2
g_push 2
g_pushglobal sequence$ll3
g_mkap 2
g_push 2
g_eval
g_proj 1
g_push 0
g_slide 1
g_updap 2, 4
g_pop 3
g_unwind

g_globstart sequence$ll3, 2
g_push 1
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal gm$cons_0_0
g_push 1
g_eval
g_proj 0
g_push 0
g_slide 1
g_updap 1, 3
g_pop 2
g_unwind
g_label .1
g_uncons 2
g_push 1
g_push 3
g_pushglobal sequence$ll2
g_mkap 2
g_push 1
g_push 4
g_eval
g_proj 1
g_push 0
g_slide 1
g_updap 2, 5
g_pop 4
g_unwind
g_label .2

g_globstart traverse_$ll1, 3
g_push 2
g_push 2
g_mkap 1
g_push 1
g_pushglobal semi$ll2
g_updap 2, 4
g_pop 3
g_unwind

g_globstart dict$Monad$IO$ll2, 1
g_push 0
g_pushglobal gm$cons_0_2
g_updap 1, 2
g_pop 1
g_unwind

g_globstart dict$Monad$IO$ll3, 3
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

g_globstart dict$Monad$IO$ll4, 2
g_push 1
g_push 1
g_pushglobal dict$Monad$IO$ll3
g_updap 2, 3
g_pop 2
g_unwind

g_globstart io$ll1, 3
g_push 1
g_push 1
g_mkap 1
g_push 3
g_push 1
g_cons 0, 2
g_push 1
g_pushglobal gm$seq
g_updap 2, 5
g_pop 4
g_unwind

g_globstart io$ll2, 2
g_push 1
g_push 1
g_pushglobal io$ll1
g_updap 2, 3
g_pop 2
g_unwind

g_globstart print$ll1, 0
g_pushglobal gm$puti
g_pushglobal io$ll2
g_updap 1, 1
g_unwind

g_globstart dict$Foldable$BinTree$ll1, 3
g_push 2
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 2
g_update 2
g_pop 1
g_unwind
g_label .1
g_uncons 3
g_push 0
g_push 3
g_push 6
g_push 6
g_pushglobal dict$Foldable$BinTree
g_eval
g_proj 0
g_push 0
g_slide 1
g_mkap 3
g_push 3
g_push 6
g_mkap 2
g_push 5
g_pushglobal dict$Foldable$BinTree
g_eval
g_proj 0
g_push 0
g_slide 1
g_updap 3, 7
g_pop 6
g_unwind
g_label .2

g_globstart dict$Foldable$BinTree$ll2, 3
g_push 2
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 2
g_update 2
g_pop 1
g_unwind
g_label .1
g_uncons 3
g_push 2
g_push 2
g_push 2
g_push 7
g_push 7
g_pushglobal dict$Foldable$BinTree
g_eval
g_proj 1
g_push 0
g_slide 1
g_mkap 3
g_push 6
g_mkap 2
g_push 5
g_pushglobal dict$Foldable$BinTree
g_eval
g_proj 1
g_push 0
g_slide 1
g_updap 3, 7
g_pop 6
g_unwind
g_label .2

g_globstart bag_insert$ll1, 4
g_push 3
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal gm$cons_0_0
g_push 3
g_pushglobal gm$cons_0_0
g_updcons 1, 3, 5
g_pop 4
g_return
g_jump .2
g_label .1
g_uncons 3
g_push 1
g_push 6
g_push 6
g_eval
g_proj 3
g_push 0
g_slide 1
g_mkap 2
g_eval
g_jumpcase .3, .4
g_label .3
g_pop 1
g_push 2
g_push 6
g_push 6
g_push 6
g_mkap 3
g_push 2
g_push 2
g_updcons 1, 3, 8
g_pop 7
g_return
g_jump .5
g_label .4
g_pop 1
g_push 2
g_push 2
g_push 2
g_push 8
g_push 8
g_push 8
g_mkap 3
g_updcons 1, 3, 8
g_pop 7
g_return
g_jump .5
g_label .5
g_jump .2
g_label .2

g_globstart tsort$ll1, 2
g_alloc 1
g_push 0
g_pushglobal bag_insert$ll1
g_updap 1, 1
g_push 1
g_push 3
g_pushglobal dict$Ord$Int
g_push 3
g_updap 3, 4
g_pop 3
g_unwind

g_globstart main$ll1, 1
g_pushglobal gm$cons_1_2
g_pushglobal gm$cons_0_0
g_push 2
g_pushglobal gm$cons_0_0
g_pushglobal tsort$ll1
g_pushglobal dict$Foldable$List$ll2
g_mkap 3
g_push 0
g_push 2
g_push 4
g_pushglobal dict$Foldable$BinTree$ll1
g_mkap 3
g_slide 3
g_pushglobal gm$cons_0_0
g_pushglobal gm$cons_0_2
g_mkap 1
g_pushglobal print$ll1
g_pushglobal dict$Monad$IO
g_pushglobal traverse_$ll1
g_mkap 2
g_pushglobal dict$Foldable$List$ll1
g_updap 3, 2
g_pop 1
g_unwind

g_globstart main$ll2, 1
g_pushglobal input
g_push 1
g_pushglobal replicate$ll1
g_mkap 2
g_pushglobal dict$Monad$IO
g_pushglobal sequence$ll3
g_mkap 2
g_pushglobal main$ll1
g_push 1
g_pushglobal dict$Monad$IO$ll3
g_updap 2, 3
g_pop 2
g_unwind
