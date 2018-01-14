external abort : ∀a. a = "abort"
type Unit =
       | Unit
type Pair a b =
       | Pair a b
type Bool =
       | False
       | True
type Choice a b =
       | First a
       | Second b
type Int
external (+) : Int -> Int -> Int = "add"
external (-) : Int -> Int -> Int = "sub"
external (*) : Int -> Int -> Int = "mul"
external (<) : Int -> Int -> Bool = "lt"
external (<=) : Int -> Int -> Bool = "le"
type List a =
       | Nil
       | Cons a (List a)
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
type IO a
external print : Int -> IO Unit = "print"
external input : IO Int = "input"
external (>>=) : ∀a b. IO a -> (a -> IO b) -> IO b = "bind"
type Option a =
       | None
       | Some a
let prime : Int = (+) ((*) 1000000 1000000) 39
let add_mod_prime : Int -> Int -> Int =
      fun (x : Int) (y : Int) ->
        let z : Int = (+) x y in
        match (<) z prime with
        | False -> (-) z prime
        | True -> z
let fibs0 : List Int = Cons @Int 0 fibs1
let fibs1 : List Int =
      Cons @Int 1 (zip_with @Int @Int @Int add_mod_prime fibs0 fibs1)
let main$ll1 : Int -> IO Unit =
      fun (n : Int) -> print (nth @Int fibs0 n)
let main : IO Unit = (>>=) @Int @Unit input main$ll1
