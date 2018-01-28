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
external (%) : Int -> Int -> Int = "mod"
external (!=) : Int -> Int -> Bool = "ne"
external (<=) : Int -> Int -> Bool = "le"
type List a =
       | Nil
       | Cons a (List a)
nth : ∀a. List a -> Int -> a =
  fun @a ->
    fun (xs : List a) (n : Int) ->
      match xs with
      | Nil @a -> abort @a
      | Cons @a x xs ->
        match (<=) n 0 with
        | False -> nth @a xs ((-) n 1)
        | True -> x
append : ∀a. List a -> List a -> List a =
  fun @a ->
    fun (xs : List a) (ys : List a) ->
      match xs with
      | Nil @a -> ys
      | Cons @a x xs -> Cons @a x (append @a xs ys)
type IO a
external print : Int -> IO Unit = "print"
external input : IO Int = "input"
external (>>=) : ∀a b. IO a -> (a -> IO b) -> IO b = "bind"
type Option a =
       | None
       | Some a
repeat : ∀a. List a -> List a =
  fun @a ->
    fun (xs : List a) ->
      let rec ys : List a = append @a xs ys in
      ys
psums$ll1 : (Int -> List Int -> List Int) -> Int -> List Int -> List Int =
  fun (psums0 : Int -> List Int -> List Int) (n : Int) (xs : List Int) ->
    match xs with
    | Nil @Int -> Nil @Int
    | Cons @Int x xs ->
      let y : Int = (+) x n in
      Cons @Int y (psums0 y xs)
psums : List Int -> List Int =
  let rec psums0 : Int -> List Int -> List Int = psums$ll1 psums0 in
  psums0 0
filter$ll1 : ∀a. (a -> Bool) -> (List a -> List a) -> List a -> List a =
  fun @a ->
    fun (p : a -> Bool) (filter_p : List a -> List a) (xs : List a) ->
      match xs with
      | Nil @a -> Nil @a
      | Cons @a x xs ->
        let ys : List a = filter_p xs in
        match p x with
        | False -> ys
        | True -> Cons @a x ys
filter : ∀a. (a -> Bool) -> List a -> List a =
  fun @a ->
    fun (p : a -> Bool) ->
      let rec filter_p : List a -> List a = filter$ll1 @a p filter_p in
      filter_p
sieve$ll1 : Int -> Int -> Bool =
  fun (p : Int) (k : Int) -> (!=) ((%) k p) 0
sieve : List Int -> List Int =
  fun (ks : List Int) ->
    match ks with
    | Nil @Int -> abort @(List Int)
    | Cons @Int p ks ->
      Cons @Int p (sieve (filter @Int (sieve$ll1 p) ks))
primes : List Int =
  Cons @Int 2 (Cons @Int 3 (sieve (psums (Cons @Int 5 (repeat @Int (Cons @Int 2 (Cons @Int 4 (Nil @Int))))))))
main$ll1 : Int -> IO Unit =
  fun (n : Int) -> print (nth @Int primes n)
main : IO Unit = (>>=) @Int @Unit input main$ll1
