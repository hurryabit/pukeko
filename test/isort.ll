external (-) = "sub"
external (<=) = "le"
let foldr f y0 xs =
      match xs with
      | Nil -> y0
      | Cons x xs -> f x (foldr f y0 xs)
let replicate n x =
      if (<=) n 0 then Nil else Cons x (replicate ((-) n 1) x)
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
let insert y xs =
      match xs with
      | Nil -> Cons y Nil
      | Cons x xs' ->
        if (<=) y x then Cons y xs else Cons x (insert y xs')
let isort xs =
      match xs with
      | Nil -> Nil
      | Cons x xs -> insert x (isort xs)
let main$ll2 xs = iter_io print (isort xs)
let main$ll1 n = (>>=) (sequence_io (replicate n input)) main$ll2
let main = (>>=) input main$ll1
