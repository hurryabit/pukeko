external (-) = "sub"
external (*) = "mul"
external (%) = "mod"
external (<) = "lt"
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
let zip_with =
      fun f xs ys ->
        match xs with
        | Nil -> Nil
        | Cons x xs ->
          match ys with
          | Nil -> Nil
          | Cons y ys -> Cons (f x y) (zip_with f xs ys)
external return = "return"
external print = "print"
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
let gen = fun f x -> Cons x (gen f (f x))
let split_at =
      fun n xs ->
        match (<=) n 0 with
        | False ->
          match xs with
          | Nil -> Pair Nil Nil
          | Cons x xs ->
            match split_at ((-) n 1) xs with
            | Pair ys zs -> Pair (Cons x ys) zs
        | True -> Pair Nil xs
let random$ll1 = fun x -> (%) ((*) 91 x) 1000000007
let random = gen random$ll1 1
let main$ll1 =
      fun n y z ->
        let y = (%) y n in
        let z = (%) z n in
        match (<) y z with
        | False -> (;) (print z) (print y)
        | True -> (;) (print y) (print z)
let main$ll2 = fun x -> return Unit
let main =
      let n = 400000 in
      (;) (print n) let m = 100000 in
                    (;) (print m) (match split_at n random with
                                   | Pair xs random ->
                                     (;) (iter_io print xs) (match split_at m random with
                                                             | Pair ys random ->
                                                               let zs = take m random in
                                                               (>>=) (sequence_io (zip_with (main$ll1 n) ys zs)) main$ll2))
