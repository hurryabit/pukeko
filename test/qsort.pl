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
ordInt : Ord Int = .Ord @Int ge_int gt_int le_int lt_int
ringInt : Ring Int = .Ring @Int neg_int add_int sub_int mul_int
foldableList : Foldable List =
  .Foldable @List foldableList.foldr.L1 foldableList.foldl.L1
monadIO : Monad IO = .Monad @IO monadIO.pure.L2 monadIO.bind.L2
input : IO Int = io.L2 @Unit @Int geti Unit
main : IO Unit = bind.L1 @IO monadIO @Int @Unit input main.L2
le.L1 : ∀a. Ord a -> a -> a -> Bool =
  fun @a (dict : Ord a) ->
    match dict with
    | .Ord _ _ le _ -> le
lt.L1 : ∀a. Ord a -> a -> a -> Bool =
  fun @a (dict : Ord a) ->
    match dict with
    | .Ord _ _ _ lt -> lt
append.L1 : ∀m. Monoid m -> m -> m -> m =
  fun @m (dict : Monoid m) ->
    match dict with
    | .Monoid _ append -> append
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
monoidList.L1 : ∀a. Monoid (List a) =
  fun @a ->
    .Monoid @(List a) (monoidList.empty.L1 @a) (monoidList.append.L1 @a)
monoidList.empty.L1 : ∀a. List a = Nil
monoidList.append.L1 : ∀a. List a -> List a -> List a =
  fun @a (xs : List a) (ys : List a) ->
    foldr.L1 @List foldableList @a @(List a) (Cons @a) ys xs
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
replicate.L1 : ∀a. Int -> a -> List a =
  fun @a (n : Int) (x : a) ->
    match le.L1 @Int ordInt n 0 with
    | False -> Cons @a x (replicate.L1 @a (sub.L1 @Int ringInt n 1) x)
    | True -> Nil @a
partition.L1 : ∀a. (a -> Bool) -> List a -> Pair (List a) (List a) =
  fun @a (p : a -> Bool) (xs : List a) ->
    match xs with
    | Nil -> Pair @(List a) @(List a) (Nil @a) (Nil @a)
    | Cons x xs ->
      match partition.L1 @a p xs with
      | Pair ys zs ->
        match p x with
        | False -> Pair @(List a) @(List a) ys (Cons @a x zs)
        | True -> Pair @(List a) @(List a) (Cons @a x ys) zs
pure.L1 : ∀m. Monad m -> (∀a. a -> m a) =
  fun @m (dict : Monad m) ->
    match dict with
    | .Monad pure _ -> pure
bind.L1 : ∀m. Monad m -> (∀a b. m a -> (a -> m b) -> m b) =
  fun @m (dict : Monad m) ->
    match dict with
    | .Monad _ bind -> bind
semi.L1 : ∀a m. m a -> Unit -> m a =
  fun @a @m (m2 : m a) (x : Unit) -> m2
semi.L2 : ∀a m. Monad m -> m Unit -> m a -> m a =
  fun @a @m (monad.m : Monad m) (m1 : m Unit) (m2 : m a) ->
    bind.L1 @m monad.m @Unit @a m1 (semi.L1 @a @m m2)
sequence.L1 : ∀a m. Monad m -> a -> List a -> m (List a) =
  fun @a @m (monad.m : Monad m) (x : a) (xs : List a) ->
    pure.L1 @m monad.m @(List a) (Cons @a x xs)
sequence.L2 : ∀a m. Monad m -> List (m a) -> a -> m (List a) =
  fun @a @m (monad.m : Monad m) (ms : List (m a)) (x : a) ->
    bind.L1 @m monad.m @(List a) @(List a) (sequence.L3 @a @m monad.m ms) (sequence.L1 @a @m monad.m x)
sequence.L3 : ∀a m. Monad m -> List (m a) -> m (List a) =
  fun @a @m (monad.m : Monad m) (ms : List (m a)) ->
    match ms with
    | Nil -> pure.L1 @m monad.m @(List a) (Nil @a)
    | Cons m ms ->
      bind.L1 @m monad.m @a @(List a) m (sequence.L2 @a @m monad.m ms)
traverse_.L1 : ∀a m. Monad m -> (a -> m Unit) -> a -> m Unit -> m Unit =
  fun @a @m (monad.m : Monad m) (f : a -> m Unit) (x : a) ->
    semi.L2 @Unit @m monad.m (f x)
traverse_.L2 : ∀a m t. Monad m -> Foldable t -> (a -> m Unit) -> t a -> m Unit =
  fun @a @m @t (monad.m : Monad m) (foldable.t : Foldable t) (f : a -> m Unit) ->
    foldr.L1 @t foldable.t @a @(m Unit) (traverse_.L1 @a @m monad.m f) (pure.L1 @m monad.m @Unit Unit)
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
qsort.L1 : Int -> Int -> Bool =
  fun (x : Int) (y : Int) -> lt.L1 @Int ordInt y x
qsort.L2 : List Int -> List Int =
  fun (xs : List Int) ->
    match xs with
    | Nil -> Nil @Int
    | Cons x xs ->
      match partition.L1 @Int (qsort.L1 x) xs with
      | Pair ys zs ->
        append.L1 @(List Int) (monoidList.L1 @Int) (qsort.L2 ys) (Cons @Int x (qsort.L2 zs))
main.L1 : List Int -> IO Unit =
  fun (xs : List Int) ->
    traverse_.L2 @Int @IO @List monadIO foldableList print.L1 (qsort.L2 xs)
main.L2 : Int -> IO Unit =
  fun (n : Int) ->
    bind.L1 @IO monadIO @(List Int) @Unit (sequence.L3 @Int @IO monadIO (replicate.L1 @(IO Int) n input)) main.L1
