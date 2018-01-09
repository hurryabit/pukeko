external (+) = "add"
external (-) = "sub"
external (==) = "eq"
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
let append =
      fun xs ys ->
        match xs with
        | Nil -> ys
        | Cons x xs -> Cons x (append xs ys)
let concat = foldr append Nil
let map =
      fun f xs ->
        match xs with
        | Nil -> Nil
        | Cons x xs -> Cons (f x) (map f xs)
let concat_map = fun f xs -> concat (map f xs)
let length$ll1 = fun x l -> (+) 1 l
let length = foldr length$ll1 0
let replicate =
      fun n x ->
        match (<=) n 0 with
        | False -> Cons x (replicate ((-) n 1) x)
        | True -> Nil
external print = "print"
external input = "input"
external (>>=) = "bind"
let diff =
      fun xs ys ->
        match xs with
        | Nil -> Nil
        | Cons x xs' ->
          match ys with
          | Nil -> xs
          | Cons y ys' ->
            match (<) x y with
            | False ->
              match (==) x y with
              | False -> diff xs ys'
              | True -> diff xs' ys'
            | True -> Cons x (diff xs' ys)
let ints$ll1 = fun go k -> Cons k (go ((+) k 1))
let ints =
      let rec go = ints$ll1 go in
      go 1
let solve_aux$ll2 =
      fun k ls i ->
        diff ls (Cons ((-) k i) (Cons k (Cons ((+) k i) Nil)))
let solve_aux$ll1 =
      fun kss k ->
        map (Cons k) (solve_aux (zip_with (solve_aux$ll2 k) kss ints))
let solve_aux =
      fun kss ->
        match kss with
        | Nil -> Cons Nil Nil
        | Cons ks kss -> concat_map (solve_aux$ll1 kss) ks
let solve = fun n -> solve_aux (replicate n (take n ints))
let main$ll1 = fun n -> print (length (solve n))
let main = (>>=) input main$ll1
