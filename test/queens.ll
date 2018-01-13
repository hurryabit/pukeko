external (+) : Int -> Int -> Int = "add"
external (-) : Int -> Int -> Int = "sub"
external (==) : Int -> Int -> Bool = "eq"
external (<) : Int -> Int -> Bool = "lt"
external (<=) : Int -> Int -> Bool = "le"
let foldr$ll1 : ∀a b. (a -> b -> b) -> b -> List a -> b =
      fun @a @b ->
        fun (f : a -> b -> b) (y0 : b) (xs : List a) ->
          match xs with
          | Nil @a -> y0
          | Cons @a x xs -> f x (foldr @a @b f y0 xs)
let foldr : ∀a b. (a -> b -> b) -> b -> List a -> b = foldr$ll1
let take$ll1 : ∀a. Int -> List a -> List a =
      fun @a ->
        fun (n : Int) (xs : List a) ->
          match (<=) n 0 with
          | False ->
            match xs with
            | Nil @a -> Nil @a
            | Cons @a x xs -> Cons @a x (take @a ((-) n 1) xs)
          | True -> Nil @a
let take : ∀a. Int -> List a -> List a = take$ll1
let zip_with$ll1 : ∀a b c. (a -> b -> c) -> List a -> List b -> List c =
      fun @a @b @c ->
        fun (f : a -> b -> c) (xs : List a) (ys : List b) ->
          match xs with
          | Nil @a -> Nil @c
          | Cons @a x xs ->
            match ys with
            | Nil @b -> Nil @c
            | Cons @b y ys -> Cons @c (f x y) (zip_with @a @b @c f xs ys)
let zip_with : ∀a b c. (a -> b -> c) -> List a -> List b -> List c =
      zip_with$ll1
let append$ll1 : ∀a. List a -> List a -> List a =
      fun @a ->
        fun (xs : List a) (ys : List a) ->
          match xs with
          | Nil @a -> ys
          | Cons @a x xs -> Cons @a x (append @a xs ys)
let append : ∀a. List a -> List a -> List a = append$ll1
let concat : ∀a. List (List a) -> List a =
      fun @a -> foldr @(List a) @(List a) (append @a) (Nil @a)
let map$ll1 : ∀a b. (a -> b) -> List a -> List b =
      fun @a @b ->
        fun (f : a -> b) (xs : List a) ->
          match xs with
          | Nil @a -> Nil @b
          | Cons @a x xs -> Cons @b (f x) (map @a @b f xs)
let map : ∀a b. (a -> b) -> List a -> List b = map$ll1
let concat_map$ll1 : ∀a b. (a -> List b) -> List a -> List b =
      fun @a @b ->
        fun (f : a -> List b) (xs : List a) ->
          concat @b (map @a @(List b) f xs)
let concat_map : ∀a b. (a -> List b) -> List a -> List b =
      concat_map$ll1
let length$ll1 : ∀a. a -> Int -> Int =
      fun @a -> fun (x : a) (l : Int) -> (+) 1 l
let length : ∀a. List a -> Int =
      fun @a -> foldr @a @Int (length$ll1 @a) 0
let replicate$ll1 : ∀a. Int -> a -> List a =
      fun @a ->
        fun (n : Int) (x : a) ->
          match (<=) n 0 with
          | False -> Cons @a x (replicate @a ((-) n 1) x)
          | True -> Nil @a
let replicate : ∀a. Int -> a -> List a = replicate$ll1
external print : Int -> IO Unit = "print"
external input : IO Int = "input"
external (>>=) : ∀a b. IO a -> (a -> IO b) -> IO b = "bind"
let diff$ll1 : List Int -> List Int -> List Int =
      fun (xs : List Int) (ys : List Int) ->
        match xs with
        | Nil @Int -> Nil @Int
        | Cons @Int x xs' ->
          match ys with
          | Nil @Int -> xs
          | Cons @Int y ys' ->
            match (<) x y with
            | False ->
              match (==) x y with
              | False -> diff xs ys'
              | True -> diff xs' ys'
            | True -> Cons @Int x (diff xs' ys)
let diff : List Int -> List Int -> List Int = diff$ll1
let ints$ll1 : (Int -> List Int) -> Int -> List Int =
      fun (go : Int -> List Int) (k : Int) -> Cons @Int k (go ((+) k 1))
let ints : List Int =
      let rec go : Int -> List Int = ints$ll1 go in
      go 1
let solve_aux$ll1 : Int -> List Int -> Int -> List Int =
      fun (k : Int) (ls : List Int) (i : Int) ->
        diff ls (Cons @Int ((-) k i) (Cons @Int k (Cons @Int ((+) k i) (Nil @Int))))
let solve_aux$ll2 : List (List Int) -> Int -> List (List Int) =
      fun (kss : List (List Int)) (k : Int) ->
        map @(List Int) @(List Int) (Cons @Int k) (solve_aux (zip_with @(List Int) @Int @(List Int) (solve_aux$ll1 k) kss ints))
let solve_aux$ll3 : List (List Int) -> List (List Int) =
      fun (kss : List (List Int)) ->
        match kss with
        | Nil @(List Int) -> Cons @(List Int) (Nil @Int) (Nil @(List Int))
        | Cons @(List Int) ks kss ->
          concat_map @Int @(List Int) (solve_aux$ll2 kss) ks
let solve_aux : List (List Int) -> List (List Int) = solve_aux$ll3
let solve$ll1 : Int -> List (List Int) =
      fun (n : Int) ->
        solve_aux (replicate @(List Int) n (take @Int n ints))
let solve : Int -> List (List Int) = solve$ll1
let main$ll1 : Int -> IO Unit =
      fun (n : Int) -> print (length @(List Int) (solve n))
let main : IO Unit = (>>=) @Int @Unit input main$ll1
