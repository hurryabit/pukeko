external abort = "abort"
external (+) = "add"
external (-) = "sub"
external (*) = "mul"
external (<) = "lt"
external (<=) = "le"
let nth xs n =
      match xs with
      | Nil -> abort
      | Cons x xs -> if (<=) n 0 then x else nth xs ((-) n 1)
let zip_with f xs ys =
      match xs with
      | Nil -> Nil
      | Cons x xs ->
        match ys with
        | Nil -> Nil
        | Cons y ys -> Cons (f x y) (zip_with f xs ys)
external print = "print"
external input = "input"
external (>>=) = "bind"
let prime = (+) ((*) 1000000 1000000) 39
let add_mod_prime x y =
      let z = (+) x y in
      if (<) z prime then z else (-) z prime
let fibs0 = Cons 0 fibs1
let fibs1 = Cons 1 (zip_with add_mod_prime fibs0 fibs1)
let main$1 n = print (nth fibs0 n)
let main = (>>=) input main$1
