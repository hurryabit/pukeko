external (-) = "sub"
external (<=) = "le"
let foldr =
      fun f y0 xs ->
        match xs with
        | Nil -> y0
        | Cons x xs -> f x (foldr f y0 xs)
let replicate =
      fun n x ->
        match (<=) n 0 with
        | False -> Cons x (replicate ((-) n 1) x)
        | True -> Nil
external return = "return"
external print = "print"
external input = "input"
external (>>=) = "bind"
let (;ll1) = fun m2 x -> m2
let (;) = fun m1 m2 -> (>>=) m1 ((;ll1) m2)
let sequence_io$ll2 = fun x xs -> return (Cons x xs)
let sequence_io$ll1 =
      fun ms x -> (>>=) (sequence_io ms) (sequence_io$ll2 x)
let sequence_io =
      fun ms ->
        match ms with
        | Nil -> return Nil
        | Cons m ms -> (>>=) m (sequence_io$ll1 ms)
let iter_io$ll1 = fun f x m -> (;) (f x) m
let iter_io = fun f -> foldr (iter_io$ll1 f) (return Unit)
let insert =
      fun y xs ->
        match xs with
        | Nil -> Cons y Nil
        | Cons x xs' ->
          match (<=) y x with
          | False -> Cons x (insert y xs')
          | True -> Cons y xs
let isort =
      fun xs ->
        match xs with
        | Nil -> Nil
        | Cons x xs -> insert x (isort xs)
let main$ll2 = fun xs -> iter_io print (isort xs)
let main$ll1 =
      fun n -> (>>=) (sequence_io (replicate n input)) main$ll2
let main = (>>=) input main$ll1
