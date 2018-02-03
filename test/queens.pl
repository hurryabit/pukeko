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
data Dict$Eq a =
       | Dict$Eq (a -> a -> Bool)
eq : ∀a. Dict$Eq a -> a -> a -> Bool =
  fun @a ->
    fun (dict : Dict$Eq a) ->
      match dict with
      | Dict$Eq @a eq -> eq
data Dict$Ord a =
       | Dict$Ord (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool)
le : ∀a. Dict$Ord a -> a -> a -> Bool =
  fun @a ->
    fun (dict : Dict$Ord a) ->
      match dict with
      | Dict$Ord @a _ _ le _ -> le
lt : ∀a. Dict$Ord a -> a -> a -> Bool =
  fun @a ->
    fun (dict : Dict$Ord a) ->
      match dict with
      | Dict$Ord @a _ _ _ lt -> lt
data Dict$Monoid m =
       | Dict$Monoid m (m -> m -> m)
empty : ∀m. Dict$Monoid m -> m =
  fun @m ->
    fun (dict : Dict$Monoid m) ->
      match dict with
      | Dict$Monoid @m empty _ -> empty
append : ∀m. Dict$Monoid m -> m -> m -> m =
  fun @m ->
    fun (dict : Dict$Monoid m) ->
      match dict with
      | Dict$Monoid @m _ append -> append
data Dict$Ring a =
       | Dict$Ring (a -> a) (a -> a -> a) (a -> a -> a) (a -> a -> a)
add : ∀a. Dict$Ring a -> a -> a -> a =
  fun @a ->
    fun (dict : Dict$Ring a) ->
      match dict with
      | Dict$Ring @a _ add _ _ -> add
sub : ∀a. Dict$Ring a -> a -> a -> a =
  fun @a ->
    fun (dict : Dict$Ring a) ->
      match dict with
      | Dict$Ring @a _ _ sub _ -> sub
data Int
external eq_int : Int -> Int -> Bool = "eq"
dict$Eq$Int : Dict$Eq Int =
  let eq : Int -> Int -> Bool = eq_int in
  Dict$Eq @Int eq
external lt_int : Int -> Int -> Bool = "lt"
external le_int : Int -> Int -> Bool = "le"
external ge_int : Int -> Int -> Bool = "ge"
external gt_int : Int -> Int -> Bool = "gt"
dict$Ord$Int : Dict$Ord Int =
  let ge : Int -> Int -> Bool = ge_int
  and gt : Int -> Int -> Bool = gt_int
  and le : Int -> Int -> Bool = le_int
  and lt : Int -> Int -> Bool = lt_int
  in
  Dict$Ord @Int ge gt le lt
external neg_int : Int -> Int = "neg"
external add_int : Int -> Int -> Int = "add"
external sub_int : Int -> Int -> Int = "sub"
external mul_int : Int -> Int -> Int = "mul"
dict$Ring$Int : Dict$Ring Int =
  let neg : Int -> Int = neg_int
  and add : Int -> Int -> Int = add_int
  and sub : Int -> Int -> Int = sub_int
  and mul : Int -> Int -> Int = mul_int
  in
  Dict$Ring @Int neg add sub mul
dict$Monoid$Int : Dict$Monoid Int =
  let empty : Int = 0
  and append : Int -> Int -> Int = add_int
  in
  Dict$Monoid @Int empty append
data Char
data Dict$Foldable t =
       | Dict$Foldable (∀a b. (a -> b -> b) -> b -> t a -> b) (∀a b. (b -> a -> b) -> b -> t a -> b)
foldr : ∀t. Dict$Foldable t -> (∀a b. (a -> b -> b) -> b -> t a -> b) =
  fun @t ->
    fun (dict : Dict$Foldable t) ->
      match dict with
      | Dict$Foldable @t foldr _ -> foldr
foldl : ∀t. Dict$Foldable t -> (∀a b. (b -> a -> b) -> b -> t a -> b) =
  fun @t ->
    fun (dict : Dict$Foldable t) ->
      match dict with
      | Dict$Foldable @t _ foldl -> foldl
foldMap$ll1 : ∀a m. Dict$Monoid m -> (a -> m) -> a -> m -> m =
  fun @a @m ->
    fun (dict$Monoid$m : Dict$Monoid m) (f : a -> m) (x : a) (m : m) ->
      append @m dict$Monoid$m (f x) m
foldMap : ∀a m t. Dict$Monoid m -> Dict$Foldable t -> (a -> m) -> t a -> m =
  fun @a @m @t ->
    fun (dict$Monoid$m : Dict$Monoid m) (dict$Foldable$t : Dict$Foldable t) (f : a -> m) ->
      foldr @t dict$Foldable$t @a @m (foldMap$ll1 @a @m dict$Monoid$m f) (empty @m dict$Monoid$m)
length$ll1 : ∀a. a -> Int = fun @a -> fun (x : a) -> 1
length : ∀a t. Dict$Foldable t -> t a -> Int =
  fun @a @t ->
    fun (dict$Foldable$t : Dict$Foldable t) ->
      foldMap @a @Int @t dict$Monoid$Int dict$Foldable$t (length$ll1 @a)
data Dict$Functor f =
       | Dict$Functor (∀a b. (a -> b) -> f a -> f b)
map : ∀f. Dict$Functor f -> (∀a b. (a -> b) -> f a -> f b) =
  fun @f ->
    fun (dict : Dict$Functor f) ->
      match dict with
      | Dict$Functor @f map -> map
data List a =
       | Nil
       | Cons a (List a)
dict$Monoid$List$ll1 : ∀a. List a -> List a -> List a =
  fun @a ->
    fun (xs : List a) (ys : List a) ->
      foldr @List dict$Foldable$List @a @(List a) (Cons @a) ys xs
dict$Monoid$List : ∀a. Dict$Monoid (List a) =
  fun @a ->
    let empty : List a = Nil @a
    and append : List a -> List a -> List a = dict$Monoid$List$ll1 @a
    in
    Dict$Monoid @(List a) empty append
dict$Functor$List$ll1 : ∀a b. (a -> b) -> List a -> List b =
  fun @a @b ->
    fun (f : a -> b) (xs : List a) ->
      match xs with
      | Nil @a -> Nil @b
      | Cons @a x xs ->
        Cons @b (f x) (map @List dict$Functor$List @a @b f xs)
dict$Functor$List : Dict$Functor List =
  let map : ∀a b. (a -> b) -> List a -> List b =
        fun @a @b -> dict$Functor$List$ll1 @a @b
  in
  Dict$Functor @List map
dict$Foldable$List$ll1 : ∀a b. (a -> b -> b) -> b -> List a -> b =
  fun @a @b ->
    fun (f : a -> b -> b) (y0 : b) (xs : List a) ->
      match xs with
      | Nil @a -> y0
      | Cons @a x xs ->
        f x (foldr @List dict$Foldable$List @a @b f y0 xs)
dict$Foldable$List$ll2 : ∀a b. (b -> a -> b) -> b -> List a -> b =
  fun @a @b ->
    fun (f : b -> a -> b) (y0 : b) (xs : List a) ->
      match xs with
      | Nil @a -> y0
      | Cons @a x xs ->
        foldl @List dict$Foldable$List @a @b f (f y0 x) xs
dict$Foldable$List : Dict$Foldable List =
  let foldr : ∀a b. (a -> b -> b) -> b -> List a -> b =
        fun @a @b -> dict$Foldable$List$ll1 @a @b
  and foldl : ∀a b. (b -> a -> b) -> b -> List a -> b =
        fun @a @b -> dict$Foldable$List$ll2 @a @b
  in
  Dict$Foldable @List foldr foldl
take : ∀a. Int -> List a -> List a =
  fun @a ->
    fun (n : Int) (xs : List a) ->
      match le @Int dict$Ord$Int n 0 with
      | False ->
        match xs with
        | Nil @a -> Nil @a
        | Cons @a x xs ->
          Cons @a x (take @a (sub @Int dict$Ring$Int n 1) xs)
      | True -> Nil @a
replicate : ∀a. Int -> a -> List a =
  fun @a ->
    fun (n : Int) (x : a) ->
      match le @Int dict$Ord$Int n 0 with
      | False -> Cons @a x (replicate @a (sub @Int dict$Ring$Int n 1) x)
      | True -> Nil @a
zip_with : ∀a b c. (a -> b -> c) -> List a -> List b -> List c =
  fun @a @b @c ->
    fun (f : a -> b -> c) (xs : List a) (ys : List b) ->
      match xs with
      | Nil @a -> Nil @c
      | Cons @a x xs ->
        match ys with
        | Nil @b -> Nil @c
        | Cons @b y ys -> Cons @c (f x y) (zip_with @a @b @c f xs ys)
data Dict$Monad m =
       | Dict$Monad (∀a. a -> m a) (∀a b. m a -> (a -> m b) -> m b)
bind : ∀m. Dict$Monad m -> (∀a b. m a -> (a -> m b) -> m b) =
  fun @m ->
    fun (dict : Dict$Monad m) ->
      match dict with
      | Dict$Monad @m _ bind -> bind
external seq : ∀a b. a -> b -> b = "seq"
external puti : Int -> Unit = "puti"
external geti : Unit -> Int = "geti"
data World =
       | World
data IO a = World -> Pair a World
dict$Monad$IO$ll1 : ∀a. a -> World -> Pair a World =
  fun @a -> Pair @a @World
dict$Monad$IO$ll2 : ∀a. a -> IO a =
  fun @a ->
    fun (x : a) ->
      coerce @(World -> Pair a World -> IO a) (dict$Monad$IO$ll1 @a x)
dict$Monad$IO$ll3 : ∀a b. IO a -> (a -> IO b) -> World -> Pair b World =
  fun @a @b ->
    fun (mx : IO a) (f : a -> IO b) (world0 : World) ->
      match coerce @(IO a -> World -> Pair a World) mx world0 with
      | Pair @a @World x world1 ->
        coerce @(IO b -> World -> Pair b World) (f x) world1
dict$Monad$IO$ll4 : ∀a b. IO a -> (a -> IO b) -> IO b =
  fun @a @b ->
    fun (mx : IO a) (f : a -> IO b) ->
      coerce @(World -> Pair b World -> IO b) (dict$Monad$IO$ll3 @a @b mx f)
dict$Monad$IO : Dict$Monad IO =
  let pure : ∀a. a -> IO a = fun @a -> dict$Monad$IO$ll2 @a
  and bind : ∀a b. IO a -> (a -> IO b) -> IO b =
        fun @a @b -> dict$Monad$IO$ll4 @a @b
  in
  Dict$Monad @IO pure bind
io$ll1 : ∀a b. (a -> b) -> a -> World -> Pair b World =
  fun @a @b ->
    fun (f : a -> b) (x : a) (world : World) ->
      let y : b = f x in
      seq @b @(Pair b World) y (Pair @b @World y world)
io : ∀a b. (a -> b) -> a -> IO b =
  fun @a @b ->
    fun (f : a -> b) (x : a) ->
      coerce @(World -> Pair b World -> IO b) (io$ll1 @a @b f x)
print : Int -> IO Unit = fun (n : Int) -> io @Int @Unit puti n
input : IO Int = io @Unit @Int geti Unit
diff : List Int -> List Int -> List Int =
  fun (xs : List Int) (ys : List Int) ->
    match xs with
    | Nil @Int -> Nil @Int
    | Cons @Int x xs' ->
      match ys with
      | Nil @Int -> xs
      | Cons @Int y ys' ->
        match lt @Int dict$Ord$Int x y with
        | False ->
          match eq @Int dict$Eq$Int x y with
          | False -> diff xs ys'
          | True -> diff xs' ys'
        | True -> Cons @Int x (diff xs' ys)
ints$ll1 : (Int -> List Int) -> Int -> List Int =
  fun (go : Int -> List Int) (k : Int) ->
    Cons @Int k (go (add @Int dict$Ring$Int k 1))
ints : List Int =
  let rec go : Int -> List Int = ints$ll1 go in
  go 1
solve_aux$ll1 : Int -> List Int -> Int -> List Int =
  fun (k : Int) (ls : List Int) (i : Int) ->
    diff ls (Cons @Int (sub @Int dict$Ring$Int k i) (Cons @Int k (Cons @Int (add @Int dict$Ring$Int k i) (Nil @Int))))
solve_aux$ll2 : List (List Int) -> Int -> List (List Int) =
  fun (kss : List (List Int)) (k : Int) ->
    map @List dict$Functor$List @(List Int) @(List Int) (Cons @Int k) (solve_aux (zip_with @(List Int) @Int @(List Int) (solve_aux$ll1 k) kss ints))
solve_aux : List (List Int) -> List (List Int) =
  fun (kss : List (List Int)) ->
    match kss with
    | Nil @(List Int) -> Cons @(List Int) (Nil @Int) (Nil @(List Int))
    | Cons @(List Int) ks kss ->
      foldMap @Int @(List (List Int)) @List (dict$Monoid$List @(List Int)) dict$Foldable$List (solve_aux$ll2 kss) ks
solve : Int -> List (List Int) =
  fun (n : Int) ->
    solve_aux (replicate @(List Int) n (take @Int n ints))
main$ll1 : Int -> IO Unit =
  fun (n : Int) ->
    print (length @(List Int) @List dict$Foldable$List (solve n))
main : IO Unit = bind @IO dict$Monad$IO @Int @Unit input main$ll1
