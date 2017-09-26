external (-) = "sub"
external (<=) = "le"
let foldr f y0 xs =
      match xs with
      | Nil -> y0
      | Cons x xs -> f x (foldr f y0 xs)
let foldl f y0 xs =
      match xs with
      | Nil -> y0
      | Cons x xs -> foldl f (f y0 x) xs
let append xs ys =
      match xs with
      | Nil -> ys
      | Cons x xs -> Cons x (append xs ys)
let replicate n x =
      match (<=) n 0 with
      | False -> Cons x (replicate ((-) n 1) x)
      | True -> Nil
let insert_tree x t =
      match t with
      | Leaf -> Branch Leaf x Leaf
      | Branch l y r ->
        match (<=) x y with
        | False -> Branch l y (insert_tree x r)
        | True -> Branch (insert_tree x l) y r
let in_order t =
      match t with
      | Leaf -> Nil
      | Branch l x r -> append (in_order l) (Cons x (in_order r))
external return = "return"
external print = "print"
external input = "input"
external (>>=) = "bind"
let (;ll1) m2 _ = m2
let (;) m1 m2 = (>>=) m1 ((;ll1) m2)
let sequence_io$ll2 x xs = return (Cons x xs)
let sequence_io$ll1 ms x =
      (>>=) (sequence_io ms) (sequence_io$ll2 x)
let sequence_io ms =
      match ms with
      | Nil -> return Nil
      | Cons m ms -> (>>=) m (sequence_io$ll1 ms)
let iter_io$ll1 f x m = (;) (f x) m
let iter_io f = foldr (iter_io$ll1 f) (return Unit)
let tsort$ll1 t x = insert_tree x t
let tsort xs = in_order (foldl tsort$ll1 Leaf xs)
let main$ll2 xs = iter_io print (tsort xs)
let main$ll1 n = (>>=) (sequence_io (replicate n input)) main$ll2
let main = (>>=) input main$ll1
