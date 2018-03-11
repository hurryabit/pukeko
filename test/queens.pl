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
       | Dict$Eq (a -> a -> Bool)
data Ord a =
       | Dict$Ord (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool)
data Monoid m =
       | Dict$Monoid m (m -> m -> m)
data Ring a =
       | Dict$Ring (a -> a) (a -> a -> a) (a -> a -> a) (a -> a -> a)
data Char
data Foldable t =
       | Dict$Foldable (∀a b. (a -> b -> b) -> b -> t a -> b) (∀a b. (b -> a -> b) -> b -> t a -> b)
data Functor f =
       | Dict$Functor (∀a b. (a -> b) -> f a -> f b)
data List a =
       | Nil
       | Cons a (List a)
data Monad m =
       | Dict$Monad (∀a. a -> m a) (∀a b. m a -> (a -> m b) -> m b)
data World =
       | World
data IO a = World -> Pair a World
external eq_int : Int -> Int -> Bool = "eq"
external lt_int : Int -> Int -> Bool = "lt"
external le_int : Int -> Int -> Bool = "le"
external add_int : Int -> Int -> Int = "add"
external sub_int : Int -> Int -> Int = "sub"
external seq : ∀a b. a -> b -> b = "seq"
external puti : Int -> Unit = "puti"
external geti : Unit -> Int = "geti"
dict$Monoid$Int : Monoid Int = Dict$Monoid @Int 0 add_int
input : IO Int = coerce @(_ -> IO) (io$ll1 @Unit @Int geti Unit)
ints : List Int =
  let rec go : Int -> List Int = ints$ll1 go in
  go 1
main : IO Unit =
  coerce @(_ -> IO) (dict$Monad$IO$ll3 @Int @Unit input main$ll1)
foldMap$ll1 : ∀a m. Monoid m -> (a -> m) -> a -> m -> m =
  fun @a @m (dict$Monoid$m : Monoid m) (f : a -> m) (x : a) ->
    (match dict$Monoid$m with
     | Dict$Monoid _ append -> append) (f x)
length$ll1 : ∀a. a -> Int = fun @a (x : a) -> 1
dict$Monoid$List$ll1 : ∀a. List a -> List a -> List a =
  fun @a (xs : List a) (ys : List a) ->
    dict$Foldable$List$ll1 @a @(List a) (Cons @a) ys xs
dict$Functor$List$ll1 : ∀a b. (a -> b) -> List a -> List b =
  fun @a @b (f : a -> b) (xs : List a) ->
    match xs with
    | Nil -> Nil @b
    | Cons x xs -> Cons @b (f x) (dict$Functor$List$ll1 @a @b f xs)
dict$Foldable$List$ll1 : ∀a b. (a -> b -> b) -> b -> List a -> b =
  fun @a @b (f : a -> b -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> y0
    | Cons x xs -> f x (dict$Foldable$List$ll1 @a @b f y0 xs)
take$ll1 : ∀a. Int -> List a -> List a =
  fun @a (n : Int) (xs : List a) ->
    match le_int n 0 with
    | False ->
      match xs with
      | Nil -> Nil @a
      | Cons x xs -> Cons @a x (take$ll1 @a (sub_int n 1) xs)
    | True -> Nil @a
replicate$ll1 : ∀a. Int -> a -> List a =
  fun @a (n : Int) (x : a) ->
    match le_int n 0 with
    | False -> Cons @a x (replicate$ll1 @a (sub_int n 1) x)
    | True -> Nil @a
zip_with$ll1 : ∀a b c. (a -> b -> c) -> List a -> List b -> List c =
  fun @a @b @c (f : a -> b -> c) (xs : List a) (ys : List b) ->
    match xs with
    | Nil -> Nil @c
    | Cons x xs ->
      match ys with
      | Nil -> Nil @c
      | Cons y ys -> Cons @c (f x y) (zip_with$ll1 @a @b @c f xs ys)
dict$Monad$IO$ll3 : ∀a b. IO a -> (a -> IO b) -> World -> Pair b World =
  fun @a @b (mx : IO a) (f : a -> IO b) (world0 : World) ->
    match coerce @(IO -> _) mx world0 with
    | Pair x world1 -> coerce @(IO -> _) (f x) world1
io$ll1 : ∀a b. (a -> b) -> a -> World -> Pair b World =
  fun @a @b (f : a -> b) (x : a) (world : World) ->
    let y : b = f x in
    seq @b @(Pair b World) y (Pair @b @World y world)
io$ll2 : ∀a b. (a -> b) -> a -> IO b =
  fun @a @b (f : a -> b) (x : a) ->
    coerce @(_ -> IO) (io$ll1 @a @b f x)
print$ll1 : Int -> IO Unit = io$ll2 @Int @Unit puti
diff$ll1 : List Int -> List Int -> List Int =
  fun (xs : List Int) (ys : List Int) ->
    match xs with
    | Nil -> Nil @Int
    | Cons x xs' ->
      match ys with
      | Nil -> xs
      | Cons y ys' ->
        match lt_int x y with
        | False ->
          match eq_int x y with
          | False -> diff$ll1 xs ys'
          | True -> diff$ll1 xs' ys'
        | True -> Cons @Int x (diff$ll1 xs' ys)
ints$ll1 : (Int -> List Int) -> Int -> List Int =
  fun (go : Int -> List Int) (k : Int) ->
    Cons @Int k (go (add_int k 1))
solve_aux$ll1 : Int -> List Int -> Int -> List Int =
  fun (k : Int) (ls : List Int) (i : Int) ->
    diff$ll1 ls (Cons @Int (sub_int k i) (Cons @Int k (Cons @Int (add_int k i) (Nil @Int))))
solve_aux$ll2 : List (List Int) -> Int -> List (List Int) =
  fun (kss : List (List Int)) (k : Int) ->
    dict$Functor$List$ll1 @(List Int) @(List Int) (Cons @Int k) (solve_aux$ll3 (zip_with$ll1 @(List Int) @Int @(List Int) (solve_aux$ll1 k) kss ints))
solve_aux$ll3 : List (List Int) -> List (List Int) =
  fun (kss : List (List Int)) ->
    match kss with
    | Nil -> Cons @(List Int) (Nil @Int) (Nil @(List Int))
    | Cons ks kss ->
      let dict$Monoid$m : Monoid (List (List Int)) =
            let empty : List (List Int) = Nil @(List Int)
            and append : List (List Int) -> List (List Int) -> List (List Int) =
                  dict$Monoid$List$ll1 @(List Int)
            in
            Dict$Monoid @(List (List Int)) empty append
      and f : Int -> List (List Int) = solve_aux$ll2 kss
      in
      dict$Foldable$List$ll1 @Int @(List (List Int)) (foldMap$ll1 @Int @(List (List Int)) dict$Monoid$m f) (match dict$Monoid$m with
                                                                                                            | Dict$Monoid empty _ ->
                                                                                                              empty) ks
main$ll1 : Int -> IO Unit =
  fun (n : Int) ->
    print$ll1 (let f : List Int -> Int = length$ll1 @(List Int) in
               dict$Foldable$List$ll1 @(List Int) @Int (foldMap$ll1 @(List Int) @Int dict$Monoid$Int f) 0 (solve_aux$ll3 (replicate$ll1 @(List Int) n (take$ll1 @Int n ints))))
