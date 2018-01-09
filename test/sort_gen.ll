external (-) = "sub"
external (*) = "mul"
external (%) = "mod"
external (<=) = "le"
let foldr =
      fun f y0 xs ->
        match xs with
        | Nil -> y0
        | Cons x xs -> f x (foldr f y0 xs)
let take =
      fun n xs ->
        match (<=) n 0 with
        | False ->
          match xs with
          | Nil -> Nil
          | Cons x xs -> Cons x (take ((-) n 1) xs)
        | True -> Nil
external return = "return"
external print = "print"
external input = "input"
external (>>=) = "bind"
let (;ll1) = fun m2 x -> m2
let (;) = fun m1 m2 -> (>>=) m1 ((;ll1) m2)
let iter_io$ll1 = fun f x m -> (;) (f x) m
let iter_io = fun f -> foldr (iter_io$ll1 f) (return Unit)
let gen = fun f x -> Cons x (gen f (f x))
let main$ll2 = fun x -> (%) ((*) 91 x) 1000000007
let main$ll1 =
      fun n -> (;) (print n) (iter_io print (take n (gen main$ll2 1)))
let main = (>>=) input main$ll1
