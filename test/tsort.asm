g_declare_globals C.0.0, 0, C.0.2, 2, C.0.4, 4, C.1.2, 2, C.1.3, 3, B.lt, 2, B.le, 2, B.ge, 2, B.gt, 2, B.sub, 2, B.seq, 2, B.puti, 1, B.geti, 1, ordInt, 0, foldableList, 0, monadIO, 0, print, 0, input, 0, foldableBinTree, 0, main, 0, foldableList.foldr.L1, 3, foldableList.foldl.L1, 3, replicate.L1, 2, semi.L1, 2, semi.L2, 3, sequence.L1, 3, sequence.L2, 3, sequence.L3, 2, traverse_.L1, 3, monadIO.pure.L2, 1, monadIO.bind.L1, 3, monadIO.bind.L2, 2, io.L1, 3, io.L2, 2, foldableBinTree.foldr.L1, 3, foldableBinTree.foldl.L1, 3, bag_insert.L1, 4, tsort.L1, 2, main.L1, 1, main.L2, 1
g_declare_main main

g_globstart C.0.0, 0
g_updcons 0, 0, 1
g_return

g_globstart C.0.2, 2
g_updcons 0, 2, 1
g_return

g_globstart C.0.4, 4
g_updcons 0, 4, 1
g_return

g_globstart C.1.2, 2
g_updcons 1, 2, 1
g_return

g_globstart C.1.3, 3
g_updcons 1, 3, 1
g_return

g_globstart B.lt, 2
g_push 1
g_eval
g_push 1
g_eval
g_les
g_update 3
g_pop 2
g_return

g_globstart B.le, 2
g_push 1
g_eval
g_push 1
g_eval
g_leq
g_update 3
g_pop 2
g_return

g_globstart B.ge, 2
g_push 1
g_eval
g_push 1
g_eval
g_geq
g_update 3
g_pop 2
g_return

g_globstart B.gt, 2
g_push 1
g_eval
g_push 1
g_eval
g_gtr
g_update 3
g_pop 2
g_return

g_globstart B.sub, 2
g_push 1
g_eval
g_push 1
g_eval
g_sub
g_update 3
g_pop 2
g_return

g_globstart B.seq, 2
g_eval
g_pop 1
g_update 1
g_unwind

g_globstart B.puti, 1
g_eval
g_print
g_updcons 0, 0, 1
g_return

g_globstart B.geti, 1
g_pop 1
g_input
g_update 1
g_return

g_globstart ordInt, 0
g_pushglobal B.lt
g_pushglobal B.le
g_pushglobal B.gt
g_pushglobal B.ge
g_updcons 0, 4, 1
g_return

g_globstart foldableList, 0
g_pushglobal foldableList.foldl.L1
g_pushglobal foldableList.foldr.L1
g_updcons 0, 2, 1
g_return

g_globstart monadIO, 0
g_pushglobal monadIO.bind.L2
g_pushglobal monadIO.pure.L2
g_updcons 0, 2, 1
g_return

g_globstart print, 0
g_pushglobal B.puti
g_pushglobal io.L2
g_updap 1, 1
g_unwind

g_globstart input, 0
g_pushglobal C.0.0
g_pushglobal B.geti
g_pushglobal io.L1
g_updap 2, 1
g_unwind

g_globstart foldableBinTree, 0
g_pushglobal foldableBinTree.foldl.L1
g_pushglobal foldableBinTree.foldr.L1
g_updcons 0, 2, 1
g_return

g_globstart main, 0
g_pushglobal main.L2
g_pushglobal input
g_pushglobal monadIO.bind.L1
g_updap 2, 1
g_unwind

g_globstart foldableList.foldr.L1, 3
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
g_pushglobal foldableList
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

g_globstart foldableList.foldl.L1, 3
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
g_pushglobal foldableList
g_eval
g_proj 1
g_push 0
g_slide 1
g_updap 3, 6
g_pop 5
g_unwind
g_label .2

g_globstart replicate.L1, 2
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
g_pushglobal B.sub
g_mkap 2
g_pushglobal replicate.L1
g_mkap 2
g_push 2
g_updcons 1, 2, 3
g_pop 2
g_return
g_jump .2
g_label .1
g_pop 1
g_pushglobal C.0.0
g_update 3
g_pop 2
g_unwind
g_label .2

g_globstart semi.L1, 2
g_update 2
g_pop 1
g_unwind

g_globstart semi.L2, 3
g_push 2
g_pushglobal semi.L1
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

g_globstart sequence.L1, 3
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

g_globstart sequence.L2, 3
g_push 2
g_push 1
g_pushglobal sequence.L1
g_mkap 2
g_push 2
g_push 2
g_pushglobal sequence.L3
g_mkap 2
g_push 2
g_eval
g_proj 1
g_push 0
g_slide 1
g_updap 2, 4
g_pop 3
g_unwind

g_globstart sequence.L3, 2
g_push 1
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal C.0.0
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
g_pushglobal sequence.L2
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

g_globstart traverse_.L1, 3
g_push 2
g_push 2
g_mkap 1
g_push 1
g_pushglobal semi.L2
g_updap 2, 4
g_pop 3
g_unwind

g_globstart monadIO.pure.L2, 1
g_push 0
g_pushglobal C.0.2
g_updap 1, 2
g_pop 1
g_unwind

g_globstart monadIO.bind.L1, 3
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

g_globstart monadIO.bind.L2, 2
g_push 1
g_push 1
g_pushglobal monadIO.bind.L1
g_updap 2, 3
g_pop 2
g_unwind

g_globstart io.L1, 3
g_push 1
g_push 1
g_mkap 1
g_push 3
g_push 1
g_cons 0, 2
g_push 1
g_pushglobal B.seq
g_updap 2, 5
g_pop 4
g_unwind

g_globstart io.L2, 2
g_push 1
g_push 1
g_pushglobal io.L1
g_updap 2, 3
g_pop 2
g_unwind

g_globstart foldableBinTree.foldr.L1, 3
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
g_pushglobal foldableBinTree
g_eval
g_proj 0
g_push 0
g_slide 1
g_mkap 3
g_push 3
g_push 6
g_mkap 2
g_push 5
g_pushglobal foldableBinTree
g_eval
g_proj 0
g_push 0
g_slide 1
g_updap 3, 7
g_pop 6
g_unwind
g_label .2

g_globstart foldableBinTree.foldl.L1, 3
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
g_pushglobal foldableBinTree
g_eval
g_proj 1
g_push 0
g_slide 1
g_mkap 3
g_push 6
g_mkap 2
g_push 5
g_pushglobal foldableBinTree
g_eval
g_proj 1
g_push 0
g_slide 1
g_updap 3, 7
g_pop 6
g_unwind
g_label .2

g_globstart bag_insert.L1, 4
g_push 3
g_eval
g_jumpcase .0, .1
g_label .0
g_pop 1
g_pushglobal C.0.0
g_push 3
g_pushglobal C.0.0
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

g_globstart tsort.L1, 2
g_alloc 1
g_push 0
g_pushglobal bag_insert.L1
g_updap 1, 1
g_push 1
g_push 3
g_pushglobal ordInt
g_push 3
g_updap 3, 4
g_pop 3
g_unwind

g_globstart main.L1, 1
g_pushglobal C.1.2
g_pushglobal C.0.0
g_push 2
g_pushglobal C.0.0
g_pushglobal tsort.L1
g_pushglobal foldableList.foldl.L1
g_mkap 3
g_push 0
g_push 2
g_push 4
g_pushglobal foldableBinTree.foldr.L1
g_mkap 3
g_slide 3
g_pushglobal C.0.0
g_pushglobal C.0.2
g_mkap 1
g_pushglobal print
g_pushglobal monadIO
g_pushglobal traverse_.L1
g_mkap 2
g_pushglobal foldableList.foldr.L1
g_updap 3, 2
g_pop 1
g_unwind

g_globstart main.L2, 1
g_pushglobal input
g_push 1
g_pushglobal replicate.L1
g_mkap 2
g_pushglobal monadIO
g_pushglobal sequence.L3
g_mkap 2
g_pushglobal main.L1
g_push 1
g_pushglobal monadIO.bind.L1
g_updap 2, 3
g_pop 2
g_unwind
