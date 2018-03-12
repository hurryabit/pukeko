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
external ge_int : Int -> Int -> Bool = "ge"
external gt_int : Int -> Int -> Bool = "gt"
external neg_int : Int -> Int = "neg"
external add_int : Int -> Int -> Int = "add"
external sub_int : Int -> Int -> Int = "sub"
external mul_int : Int -> Int -> Int = "mul"
external seq : ∀a b. a -> b -> b = "seq"
external puti : Int -> Unit = "puti"
external geti : Unit -> Int = "geti"
eqInt : Eq Int = .Eq @Int eq_int
ordInt : Ord Int = .Ord @Int ge_int gt_int le_int lt_int
ringInt : Ring Int = .Ring @Int neg_int add_int sub_int mul_int
monoidInt : Monoid Int = .Monoid @Int monoidInt.empty add_int
functorList : Functor List = .Functor @List functorList.map.L1
foldableList : Foldable List =
  .Foldable @List foldableList.foldr.L1 foldableList.foldl.L1
monadIO : Monad IO = .Monad @IO monadIO.pure.L2 monadIO.bind.L2
input : IO Int = io.L2 @Unit @Int geti Unit
ints : List Int =
  let rec go : Int -> List Int = ints.L1 go in
  go 1
main : IO Unit = bind.L1 @IO monadIO @Int @Unit input main.L1
monoidInt.empty : Int = 0
eq.L1 : ∀a. Eq a -> a -> a -> Bool =
  fun @a (dict : Eq a) ->
    match dict with
    | .Eq eq -> eq
le.L1 : ∀a. Ord a -> a -> a -> Bool =
  fun @a (dict : Ord a) ->
    match dict with
    | .Ord _ _ le _ -> le
lt.L1 : ∀a. Ord a -> a -> a -> Bool =
  fun @a (dict : Ord a) ->
    match dict with
    | .Ord _ _ _ lt -> lt
empty.L1 : ∀m. Monoid m -> m =
  fun @m (dict : Monoid m) ->
    match dict with
    | .Monoid empty _ -> empty
append.L1 : ∀m. Monoid m -> m -> m -> m =
  fun @m (dict : Monoid m) ->
    match dict with
    | .Monoid _ append -> append
add.L1 : ∀a. Ring a -> a -> a -> a =
  fun @a (dict : Ring a) ->
    match dict with
    | .Ring _ add _ _ -> add
sub.L1 : ∀a. Ring a -> a -> a -> a =
  fun @a (dict : Ring a) ->
    match dict with
    | .Ring _ _ sub _ -> sub
foldr.L1 : ∀t. Foldable t -> (∀a b. (a -> b -> b) -> b -> t a -> b) =
  fun @t (dict : Foldable t) ->
    match dict with
    | .Foldable foldr _ -> foldr
foldl.L1 : ∀t. Foldable t -> (∀a b. (b -> a -> b) -> b -> t a -> b) =
  fun @t (dict : Foldable t) ->
    match dict with
    | .Foldable _ foldl -> foldl
foldMap.L1 : ∀a m. Monoid m -> (a -> m) -> a -> m -> m =
  fun @a @m (monoid.m : Monoid m) (f : a -> m) (x : a) ->
    append.L1 @m monoid.m (f x)
foldMap.L2 : ∀a m t. Foldable t -> Monoid m -> (a -> m) -> t a -> m =
  fun @a @m @t (foldable.t : Foldable t) (monoid.m : Monoid m) (f : a -> m) ->
    foldr.L1 @t foldable.t @a @m (foldMap.L1 @a @m monoid.m f) (empty.L1 @m monoid.m)
length.L1 : ∀a. a -> Int = fun @a (x : a) -> 1
length.L2 : ∀a t. Foldable t -> t a -> Int =
  fun @a @t (foldable.t : Foldable t) ->
    foldMap.L2 @a @Int @t foldable.t monoidInt (length.L1 @a)
map.L1 : ∀f. Functor f -> (∀a b. (a -> b) -> f a -> f b) =
  fun @f (dict : Functor f) ->
    match dict with
    | .Functor map -> map
monoidList.L1 : ∀a. Monoid (List a) =
  fun @a ->
    .Monoid @(List a) (monoidList.empty.L1 @a) (monoidList.append.L1 @a)
monoidList.empty.L1 : ∀a. List a = Nil
monoidList.append.L1 : ∀a. List a -> List a -> List a =
  fun @a (xs : List a) (ys : List a) ->
    foldr.L1 @List foldableList @a @(List a) (Cons @a) ys xs
functorList.map.L1 : ∀a b. (a -> b) -> List a -> List b =
  fun @a @b (f : a -> b) (xs : List a) ->
    match xs with
    | Nil -> Nil @b
    | Cons x xs -> Cons @b (f x) (map.L1 @List functorList @a @b f xs)
foldableList.foldr.L1 : ∀a b. (a -> b -> b) -> b -> List a -> b =
  fun @a @b (f : a -> b -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> y0
    | Cons x xs -> f x (foldr.L1 @List foldableList @a @b f y0 xs)
foldableList.foldl.L1 : ∀a b. (b -> a -> b) -> b -> List a -> b =
  fun @a @b (f : b -> a -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> y0
    | Cons x xs -> foldl.L1 @List foldableList @a @b f (f y0 x) xs
take.L1 : ∀a. Int -> List a -> List a =
  fun @a (n : Int) (xs : List a) ->
    match le.L1 @Int ordInt n 0 with
    | False ->
      match xs with
      | Nil -> Nil @a
      | Cons x xs -> Cons @a x (take.L1 @a (sub.L1 @Int ringInt n 1) xs)
    | True -> Nil @a
replicate.L1 : ∀a. Int -> a -> List a =
  fun @a (n : Int) (x : a) ->
    match le.L1 @Int ordInt n 0 with
    | False -> Cons @a x (replicate.L1 @a (sub.L1 @Int ringInt n 1) x)
    | True -> Nil @a
zip_with.L1 : ∀a b c. (a -> b -> c) -> List a -> List b -> List c =
  fun @a @b @c (f : a -> b -> c) (xs : List a) (ys : List b) ->
    match xs with
    | Nil -> Nil @c
    | Cons x xs ->
      match ys with
      | Nil -> Nil @c
      | Cons y ys -> Cons @c (f x y) (zip_with.L1 @a @b @c f xs ys)
bind.L1 : ∀m. Monad m -> (∀a b. m a -> (a -> m b) -> m b) =
  fun @m (dict : Monad m) ->
    match dict with
    | .Monad _ bind -> bind
monadIO.pure.L1 : ∀a. a -> World -> Pair a World =
  fun @a -> Pair @a @World
monadIO.pure.L2 : ∀a. a -> IO a =
  fun @a (x : a) -> coerce @(_ -> IO) (monadIO.pure.L1 @a x)
monadIO.bind.L1 : ∀a b. IO a -> (a -> IO b) -> World -> Pair b World =
  fun @a @b (mx : IO a) (f : a -> IO b) (world0 : World) ->
    match coerce @(IO -> _) mx world0 with
    | Pair x world1 -> coerce @(IO -> _) (f x) world1
monadIO.bind.L2 : ∀a b. IO a -> (a -> IO b) -> IO b =
  fun @a @b (mx : IO a) (f : a -> IO b) ->
    coerce @(_ -> IO) (monadIO.bind.L1 @a @b mx f)
io.L1 : ∀a b. (a -> b) -> a -> World -> Pair b World =
  fun @a @b (f : a -> b) (x : a) (world : World) ->
    let y : b = f x in
    seq @b @(Pair b World) y (Pair @b @World y world)
io.L2 : ∀a b. (a -> b) -> a -> IO b =
  fun @a @b (f : a -> b) (x : a) ->
    coerce @(_ -> IO) (io.L1 @a @b f x)
print.L1 : Int -> IO Unit = io.L2 @Int @Unit puti
diff.L1 : List Int -> List Int -> List Int =
  fun (xs : List Int) (ys : List Int) ->
    match xs with
    | Nil -> Nil @Int
    | Cons x xs' ->
      match ys with
      | Nil -> xs
      | Cons y ys' ->
        match lt.L1 @Int ordInt x y with
        | False ->
          match eq.L1 @Int eqInt x y with
          | False -> diff.L1 xs ys'
          | True -> diff.L1 xs' ys'
        | True -> Cons @Int x (diff.L1 xs' ys)
ints.L1 : (Int -> List Int) -> Int -> List Int =
  fun (go : Int -> List Int) (k : Int) ->
    Cons @Int k (go (add.L1 @Int ringInt k 1))
solve_aux.L1 : Int -> List Int -> Int -> List Int =
  fun (k : Int) (ls : List Int) (i : Int) ->
    diff.L1 ls (Cons @Int (sub.L1 @Int ringInt k i) (Cons @Int k (Cons @Int (add.L1 @Int ringInt k i) (Nil @Int))))
solve_aux.L2 : List (List Int) -> Int -> List (List Int) =
  fun (kss : List (List Int)) (k : Int) ->
    map.L1 @List functorList @(List Int) @(List Int) (Cons @Int k) (solve_aux.L3 (zip_with.L1 @(List Int) @Int @(List Int) (solve_aux.L1 k) kss ints))
solve_aux.L3 : List (List Int) -> List (List Int) =
  fun (kss : List (List Int)) ->
    match kss with
    | Nil -> Cons @(List Int) (Nil @Int) (Nil @(List Int))
    | Cons ks kss ->
      foldMap.L2 @Int @(List (List Int)) @List foldableList (monoidList.L1 @(List Int)) (solve_aux.L2 kss) ks
solve.L1 : Int -> List (List Int) =
  fun (n : Int) ->
    solve_aux.L3 (replicate.L1 @(List Int) n (take.L1 @Int n ints))
main.L1 : Int -> IO Unit =
  fun (n : Int) ->
    print.L1 (length.L2 @(List Int) @List foldableList (solve.L1 n))
