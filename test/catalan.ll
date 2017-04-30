external abort = "abort"
external (+) = "add"
external (-) = "sub"
external (*) = "mul"
external (%) = "mod"
external (<=) = "le"
let foldl f y0 xs =
      match xs with
      | Nil -> y0
      | Cons x xs -> foldl f (f y0 x) xs
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
let map f xs =
      match xs with
      | Nil -> Nil
      | Cons x xs -> Cons (f x) (map f xs)
external print = "print"
external input = "input"
external (>>=) = "bind"
let p = 100000007
let mul_p x y = (%) ((*) x y) p
let add_p x y = (%) ((+) x y) p
let sum_p = foldl add_p 0
let scanl$1 f scanl_f y0 xs =
      match xs with
      | Nil -> Nil
      | Cons x xs ->
        let y0 = f y0 x in
        Cons y0 (scanl_f y0 xs)
let scanl f =
      let rec scanl_f = scanl$1 f scanl_f in
      scanl_f
let sols$1 xs = sum_p (zip_with mul_p sols xs)
let sols$2 xs x = Cons x xs
let sols = Cons 1 (map sols$1 (scanl sols$2 Nil sols))
let main$1 n = print (nth sols n)
let main = (>>=) input main$1
