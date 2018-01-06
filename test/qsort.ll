external (-) = "sub"
external (<) = "lt"
external (<=) = "le"
let foldr f y0 xs =
      match xs with
      | Nil -> y0
      | Cons x xs -> f x (foldr f y0 xs)
let partition$ll1 p part_p xs =
      match xs with
      | Nil -> Pair Nil Nil
      | Cons x xs ->
        match part_p xs with
        | Pair ys zs ->
          match p x with
          | False -> Pair ys (Cons x zs)
          | True -> Pair (Cons x ys) zs
let partition p xs =
      let rec part_p = partition$ll1 p part_p in
      part_p xs
let append xs ys =
      match xs with
      | Nil -> ys
      | Cons x xs -> Cons x (append xs ys)
let replicate n x =
      match (<=) n 0 with
      | False -> Cons x (replicate ((-) n 1) x)
      | True -> Nil
external return = "return"
external print = "print"
external input = "input"
external (>>=) = "bind"
let (;ll1) m2 x = m2
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
let qsort$ll1 x y = (<) y x
let qsort xs =
      match xs with
      | Nil -> Nil
      | Cons x xs ->
        match partition (qsort$ll1 x) xs with
        | Pair ys zs -> append (qsort ys) (Cons x (qsort zs))
let main$ll2 xs = iter_io print (qsort xs)
let main$ll1 n = (>>=) (sequence_io (replicate n input)) main$ll2
let main = (>>=) input main$ll1
