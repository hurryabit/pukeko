external abort : ∀a. a = "abort"
external (+) : Int -> Int -> Int = "add"
external (-) : Int -> Int -> Int = "sub"
external (*) : Int -> Int -> Int = "mul"
external (%) : Int -> Int -> Int = "mod"
external (<=) : Int -> Int -> Bool = "le"
let foldl : ∀a b. (b -> a -> b) -> b -> List a -> b =
      fun @a @b ->
        fun (f : b -> a -> b) (y0 : b) (xs : List a) ->
          match xs with
          | Nil @a -> y0
          | Cons @a x xs -> foldl @a @b f (f y0 x) xs
let nth : ∀a. List a -> Int -> a =
      fun @a ->
        fun (xs : List a) (n : Int) ->
          match xs with
          | Nil @a -> abort @a
          | Cons @a x xs ->
            match (<=) n 0 with
            | False -> nth @a xs ((-) n 1)
            | True -> x
let zip_with : ∀a b c. (a -> b -> c) -> List a -> List b -> List c =
      fun @a @b @c ->
        fun (f : a -> b -> c) (xs : List a) (ys : List b) ->
          match xs with
          | Nil @a -> Nil @c
          | Cons @a x xs ->
            match ys with
            | Nil @b -> Nil @c
            | Cons @b y ys -> Cons @c (f x y) (zip_with @a @b @c f xs ys)
let map : ∀a b. (a -> b) -> List a -> List b =
      fun @a @b ->
        fun (f : a -> b) (xs : List a) ->
          match xs with
          | Nil @a -> Nil @b
          | Cons @a x xs -> Cons @b (f x) (map @a @b f xs)
external print : Int -> IO Unit = "print"
external input : IO Int = "input"
external (>>=) : ∀a b. IO a -> (a -> IO b) -> IO b = "bind"
let p : Int = 100000007
let mul_p : Int -> Int -> Int =
      fun (x : Int) (y : Int) -> (%) ((*) x y) p
let add_p : Int -> Int -> Int =
      fun (x : Int) (y : Int) -> (%) ((+) x y) p
let sum_p : List Int -> Int = foldl @Int @Int add_p 0
let scanl$ll1 : ∀a b. (b -> a -> b) -> (b -> List a -> List b) -> b -> List a -> List b =
      fun @a @b ->
        fun (f : b -> a -> b) (scanl_f : b -> List a -> List b) (y0 : b) (xs : List a) ->
          match xs with
          | Nil @a -> Nil @b
          | Cons @a x xs ->
            let y0 : b = f y0 x in
            Cons @b y0 (scanl_f y0 xs)
let scanl : ∀a b. (b -> a -> b) -> b -> List a -> List b =
      fun @a @b ->
        fun (f : b -> a -> b) ->
          let rec scanl_f : b -> List a -> List b = scanl$ll1 @a @b f scanl_f
          in
          scanl_f
let sols$ll1 : List Int -> Int =
      fun (xs : List Int) ->
        sum_p (zip_with @Int @Int @Int mul_p sols xs)
let sols$ll2 : List Int -> Int -> List Int =
      fun (xs : List Int) (x : Int) -> Cons @Int x xs
let sols : List Int =
      Cons @Int 1 (map @(List Int) @Int sols$ll1 (scanl @Int @(List Int) sols$ll2 (Nil @Int) sols))
let main$ll1 : Int -> IO Unit =
      fun (n : Int) -> print (nth @Int sols n)
let main : IO Unit = (>>=) @Int @Unit input main$ll1
