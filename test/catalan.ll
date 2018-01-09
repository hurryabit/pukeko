external abort = "abort"
external (+) = "add"
external (-) = "sub"
external (*) = "mul"
external (%) = "mod"
external (<=) = "le"
let foldl =
      fun f y0 xs ->
        match xs with
        | Nil -> y0
        | Cons x xs -> foldl f (f y0 x) xs
let nth =
      fun xs n ->
        match xs with
        | Nil -> abort
        | Cons x xs ->
          match (<=) n 0 with
          | False -> nth xs ((-) n 1)
          | True -> x
let zip_with =
      fun f xs ys ->
        match xs with
        | Nil -> Nil
        | Cons x xs ->
          match ys with
          | Nil -> Nil
          | Cons y ys -> Cons (f x y) (zip_with f xs ys)
let map =
      fun f xs ->
        match xs with
        | Nil -> Nil
        | Cons x xs -> Cons (f x) (map f xs)
external print = "print"
external input = "input"
external (>>=) = "bind"
let p = 100000007
let mul_p = fun x y -> (%) ((*) x y) p
let add_p = fun x y -> (%) ((+) x y) p
let sum_p = foldl add_p 0
let scanl$ll1 =
      fun f scanl_f y0 xs ->
        match xs with
        | Nil -> Nil
        | Cons x xs ->
          let y0 = f y0 x in
          Cons y0 (scanl_f y0 xs)
let scanl =
      fun f ->
        let rec scanl_f = scanl$ll1 f scanl_f in
        scanl_f
let sols$ll1 = fun xs -> sum_p (zip_with mul_p sols xs)
let sols$ll2 = fun xs x -> Cons x xs
let sols = Cons 1 (map sols$ll1 (scanl sols$ll2 Nil sols))
let main$ll1 = fun n -> print (nth sols n)
let main = (>>=) input main$ll1
