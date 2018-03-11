data Unit =
       | Unit
data Bool =
       | False
       | True
data Pair a b =
       | Pair a b
data Option a =
       | None
       | Some a
data Choice a b =
       | First a
       | Second b
data Eq a =
       | .Eq (a -> a -> Bool)
data Ord a =
       | .Ord (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool)
data Monoid m =
       | .Monoid m (m -> m -> m)
data Ring a =
       | .Ring (a -> a) (a -> a -> a) (a -> a -> a) (a -> a -> a)
data Char
data Foldable t =
       | .Foldable (∀a b. (a -> b -> b) -> b -> t a -> b) (∀a b. (b -> a -> b) -> b -> t a -> b)
data Functor f =
       | .Functor (∀a b. (a -> b) -> f a -> f b)
data List a =
       | Nil
       | Cons a (List a)
data Monad m =
       | .Monad (∀a. a -> m a) (∀a b. m a -> (a -> m b) -> m b)
data World =
       | World
data IO a = World -> Pair a World
external abort : ∀a. a = "abort"
external eq_int : Int -> Int -> Bool = "eq"
external le_int : Int -> Int -> Bool = "le"
external neg_int : Int -> Int = "neg"
external add_int : Int -> Int -> Int = "add"
external sub_int : Int -> Int -> Int = "sub"
external mul_int : Int -> Int -> Int = "mul"
external mod : Int -> Int -> Int = "mod"
external seq : ∀a b. a -> b -> b = "seq"
external puti : Int -> Unit = "puti"
external geti : Unit -> Int = "geti"
ringInt : Ring Int = .Ring @Int neg_int add_int sub_int mul_int
print : Int -> IO Unit = io.L2 @Int @Unit puti
input : IO Int = coerce @(_ -> IO) (io.L1 @Unit @Int geti Unit)
psums : List Int -> List Int =
  let rec psums0 : ∀_10. Ring _10 -> _10 -> List _10 -> List _10 =
            psums.L1 psums0
  in
  psums0 @Int ringInt 0
sieve : List Int -> List Int =
  fun (ks : List Int) ->
    match ks with
    | Nil -> abort @(List Int)
    | Cons p ks ->
      Cons @Int p (sieve (let p : Int -> Bool = sieve.L1 p in
                          let rec filter_p : List Int -> List Int = filter.L1 @Int p filter_p
                          in
                          filter_p ks))
primes : List Int =
  Cons @Int 2 (Cons @Int 3 (sieve (psums (Cons @Int 5 (let xs : List Int =
                                                             Cons @Int 2 (Cons @Int 4 (Nil @Int))
                                                       in
                                                       let rec ys : List Int =
                                                                 let dict : Monoid (List Int) =
                                                                       .Monoid @(List Int) (monoidList.empty @Int) (monoidList.append.L1 @Int)
                                                                 in
                                                                 (match dict with
                                                                  | .Monoid _ append ->
                                                                    append) xs ys
                                                       in
                                                       ys)))))
main : IO Unit =
  coerce @(_ -> IO) (monadIO.bind.L1 @Int @Unit input main.L1)
monoidList.empty : ∀a. List a = Nil
monoidList.append.L1 : ∀a. List a -> List a -> List a =
  fun @a (xs : List a) (ys : List a) ->
    foldableList.foldr.L1 @a @(List a) (Cons @a) ys xs
foldableList.foldr.L1 : ∀a b. (a -> b -> b) -> b -> List a -> b =
  fun @a @b (f : a -> b -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> y0
    | Cons x xs -> f x (foldableList.foldr.L1 @a @b f y0 xs)
nth_exn.L1 : ∀a. List a -> Int -> a =
  fun @a (xs : List a) (n : Int) ->
    match xs with
    | Nil -> abort @a
    | Cons x xs ->
      match le_int n 0 with
      | False -> nth_exn.L1 @a xs (sub_int n 1)
      | True -> x
monadIO.bind.L1 : ∀a b. IO a -> (a -> IO b) -> World -> Pair b World =
  fun @a @b (mx : IO a) (f : a -> IO b) (world0 : World) ->
    match coerce @(IO -> _) mx world0 with
    | Pair x world1 -> coerce @(IO -> _) (f x) world1
io.L1 : ∀a b. (a -> b) -> a -> World -> Pair b World =
  fun @a @b (f : a -> b) (x : a) (world : World) ->
    let y : b = f x in
    seq @b @(Pair b World) y (Pair @b @World y world)
io.L2 : ∀a b. (a -> b) -> a -> IO b =
  fun @a @b (f : a -> b) (x : a) ->
    coerce @(_ -> IO) (io.L1 @a @b f x)
psums.L1 : (∀_10. Ring _10 -> _10 -> List _10 -> List _10) -> (∀_10. Ring _10 -> _10 -> List _10 -> List _10) =
  fun (psums0 : ∀_10. Ring _10 -> _10 -> List _10 -> List _10) @_10 (ring._10 : Ring _10) (n : _10) (xs : List _10) ->
    match xs with
    | Nil -> Nil @_10
    | Cons x xs ->
      let y : _10 =
            (match ring._10 with
             | .Ring _ add _ _ -> add) x n
      in
      Cons @_10 y (psums0 @_10 ring._10 y xs)
filter.L1 : ∀a. (a -> Bool) -> (List a -> List a) -> List a -> List a =
  fun @a (p : a -> Bool) (filter_p : List a -> List a) (xs : List a) ->
    match xs with
    | Nil -> Nil @a
    | Cons x xs ->
      let ys : List a = filter_p xs in
      match p x with
      | False -> ys
      | True -> Cons @a x ys
sieve.L1 : Int -> Int -> Bool =
  fun (p : Int) (k : Int) ->
    let x : Int = mod k p in
    match eq_int x 0 with
    | False -> True
    | True -> False
main.L1 : Int -> IO Unit =
  fun (n : Int) -> print (nth_exn.L1 @Int primes n)
