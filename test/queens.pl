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
external eq_int : Int -> Int -> Bool = "eq"
external lt_int : Int -> Int -> Bool = "lt"
external le_int : Int -> Int -> Bool = "le"
external add_int : Int -> Int -> Int = "add"
external sub_int : Int -> Int -> Int = "sub"
external seq : ∀a b. a -> b -> b = "seq"
external puti : Int -> Unit = "puti"
external geti : Unit -> Int = "geti"
monoidInt : Monoid Int = .Monoid @Int monoidInt.empty add_int
functorList : Functor List = .Functor @List functorList.map.L1
foldableList : Foldable List =
  .Foldable @List foldableList.foldr.L1 foldableList.foldl.L1
print : Int -> IO Unit = io.L2 @Int @Unit puti
input : IO Int = coerce @(_ -> IO) (io.L1 @Unit @Int geti Unit)
ints : List Int =
  let rec go : Int -> List Int = ints.L1 go in
  go 1
solve_aux : List (List Int) -> List (List Int) =
  fun (kss : List (List Int)) ->
    match kss with
    | Nil -> Cons @(List Int) (Nil @Int) (Nil @(List Int))
    | Cons ks kss ->
      let monoid.m : Monoid (List (List Int)) =
            .Monoid @(List (List Int)) (monoidList.empty @(List Int)) (monoidList.append.L1 @(List Int))
      and f : Int -> List (List Int) = solve_aux.L2 kss
      in
      foldableList.foldr.L1 @Int @(List (List Int)) (foldMap.L1 @Int @(List (List Int)) monoid.m f) (match monoid.m with
                                                                                                     | .Monoid empty _ ->
                                                                                                       empty) ks
main : IO Unit =
  coerce @(_ -> IO) (monadIO.bind.L1 @Int @Unit input main.L1)
monoidInt.empty : Int = 0
monoidList.empty : ∀a. List a = Nil
foldMap.L1 : ∀a m. Monoid m -> (a -> m) -> a -> m -> m =
  fun @a @m (monoid.m : Monoid m) (f : a -> m) (x : a) ->
    (match monoid.m with
     | .Monoid _ append -> append) (f x)
length.L1 : ∀a. a -> Int = fun @a (x : a) -> 1
monoidList.append.L1 : ∀a. List a -> List a -> List a =
  fun @a (xs : List a) (ys : List a) ->
    foldableList.foldr.L1 @a @(List a) (Cons @a) ys xs
functorList.map.L1 : ∀a b. (a -> b) -> List a -> List b =
  fun @a @b (f : a -> b) (xs : List a) ->
    match xs with
    | Nil -> Nil @b
    | Cons x xs ->
      Cons @b (f x) ((match functorList with
                      | .Functor map -> map) @a @b f xs)
foldableList.foldr.L1 : ∀a b. (a -> b -> b) -> b -> List a -> b =
  fun @a @b (f : a -> b -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> y0
    | Cons x xs ->
      f x ((match foldableList with
            | .Foldable foldr _ -> foldr) @a @b f y0 xs)
foldableList.foldl.L1 : ∀a b. (b -> a -> b) -> b -> List a -> b =
  fun @a @b (f : b -> a -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> y0
    | Cons x xs ->
      (match foldableList with
       | .Foldable _ foldl -> foldl) @a @b f (f y0 x) xs
take.L1 : ∀a. Int -> List a -> List a =
  fun @a (n : Int) (xs : List a) ->
    match le_int n 0 with
    | False ->
      match xs with
      | Nil -> Nil @a
      | Cons x xs -> Cons @a x (take.L1 @a (sub_int n 1) xs)
    | True -> Nil @a
replicate.L1 : ∀a. Int -> a -> List a =
  fun @a (n : Int) (x : a) ->
    match le_int n 0 with
    | False -> Cons @a x (replicate.L1 @a (sub_int n 1) x)
    | True -> Nil @a
zip_with.L1 : ∀a b c. (a -> b -> c) -> List a -> List b -> List c =
  fun @a @b @c (f : a -> b -> c) (xs : List a) (ys : List b) ->
    match xs with
    | Nil -> Nil @c
    | Cons x xs ->
      match ys with
      | Nil -> Nil @c
      | Cons y ys -> Cons @c (f x y) (zip_with.L1 @a @b @c f xs ys)
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
diff.L1 : List Int -> List Int -> List Int =
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
          | False -> diff.L1 xs ys'
          | True -> diff.L1 xs' ys'
        | True -> Cons @Int x (diff.L1 xs' ys)
ints.L1 : (Int -> List Int) -> Int -> List Int =
  fun (go : Int -> List Int) (k : Int) ->
    Cons @Int k (go (add_int k 1))
solve_aux.L1 : Int -> List Int -> Int -> List Int =
  fun (k : Int) (ls : List Int) (i : Int) ->
    diff.L1 ls (Cons @Int (sub_int k i) (Cons @Int k (Cons @Int (add_int k i) (Nil @Int))))
solve_aux.L2 : List (List Int) -> Int -> List (List Int) =
  fun (kss : List (List Int)) (k : Int) ->
    functorList.map.L1 @(List Int) @(List Int) (Cons @Int k) (solve_aux (zip_with.L1 @(List Int) @Int @(List Int) (solve_aux.L1 k) kss ints))
main.L1 : Int -> IO Unit =
  fun (n : Int) ->
    print (let f : List Int -> Int = length.L1 @(List Int) in
           foldableList.foldr.L1 @(List Int) @Int (foldMap.L1 @(List Int) @Int monoidInt f) monoidInt.empty (solve_aux (replicate.L1 @(List Int) n (take.L1 @Int n ints))))
