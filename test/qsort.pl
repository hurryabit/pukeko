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
external lt_int : Int -> Int -> Bool = "lt"
external le_int : Int -> Int -> Bool = "le"
external sub_int : Int -> Int -> Int = "sub"
external seq : ∀a b. a -> b -> b = "seq"
external puti : Int -> Unit = "puti"
external geti : Unit -> Int = "geti"
dict$Monad$IO : Monad IO =
  Dict$Monad @IO dict$Monad$IO$ll2 dict$Monad$IO$ll4
input : IO Int = coerce @(_ -> IO) (io$ll1 @Unit @Int geti Unit)
main : IO Unit =
  coerce @(_ -> IO) (dict$Monad$IO$ll3 @Int @Unit input main$ll2)
dict$Monoid$List$ll1 : ∀a. List a -> List a -> List a =
  fun @a (xs : List a) (ys : List a) ->
    dict$Foldable$List$ll1 @a @(List a) (Cons @a) ys xs
dict$Foldable$List$ll1 : ∀a b. (a -> b -> b) -> b -> List a -> b =
  fun @a @b (f : a -> b -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> y0
    | Cons x xs -> f x (dict$Foldable$List$ll1 @a @b f y0 xs)
replicate$ll1 : ∀a. Int -> a -> List a =
  fun @a (n : Int) (x : a) ->
    match le_int n 0 with
    | False -> Cons @a x (replicate$ll1 @a (sub_int n 1) x)
    | True -> Nil @a
partition$ll1 : ∀a. (a -> Bool) -> List a -> Pair (List a) (List a) =
  fun @a (p : a -> Bool) (xs : List a) ->
    match xs with
    | Nil -> Pair @(List a) @(List a) (Nil @a) (Nil @a)
    | Cons x xs ->
      match partition$ll1 @a p xs with
      | Pair ys zs ->
        match p x with
        | False -> Pair @(List a) @(List a) ys (Cons @a x zs)
        | True -> Pair @(List a) @(List a) (Cons @a x ys) zs
semi$ll1 : ∀a m. m a -> Unit -> m a =
  fun @a @m (m2 : m a) (x : Unit) -> m2
semi$ll2 : ∀a m. Monad m -> m Unit -> m a -> m a =
  fun @a @m (dict$Monad$m : Monad m) (m1 : m Unit) (m2 : m a) ->
    (match dict$Monad$m with
     | Dict$Monad _ bind -> bind) @Unit @a m1 (semi$ll1 @a @m m2)
sequence$ll1 : ∀a m. Monad m -> a -> List a -> m (List a) =
  fun @a @m (dict$Monad$m : Monad m) (x : a) (xs : List a) ->
    (match dict$Monad$m with
     | Dict$Monad pure _ -> pure) @(List a) (Cons @a x xs)
sequence$ll2 : ∀a m. Monad m -> List (m a) -> a -> m (List a) =
  fun @a @m (dict$Monad$m : Monad m) (ms : List (m a)) (x : a) ->
    (match dict$Monad$m with
     | Dict$Monad _ bind ->
       bind) @(List a) @(List a) (sequence$ll3 @a @m dict$Monad$m ms) (sequence$ll1 @a @m dict$Monad$m x)
sequence$ll3 : ∀a m. Monad m -> List (m a) -> m (List a) =
  fun @a @m (dict$Monad$m : Monad m) (ms : List (m a)) ->
    match ms with
    | Nil ->
      (match dict$Monad$m with
       | Dict$Monad pure _ -> pure) @(List a) (Nil @a)
    | Cons m ms ->
      (match dict$Monad$m with
       | Dict$Monad _ bind ->
         bind) @a @(List a) m (sequence$ll2 @a @m dict$Monad$m ms)
traverse_$ll1 : ∀a m. Monad m -> (a -> m Unit) -> a -> m Unit -> m Unit =
  fun @a @m (dict$Monad$m : Monad m) (f : a -> m Unit) (x : a) ->
    semi$ll2 @Unit @m dict$Monad$m (f x)
dict$Monad$IO$ll2 : ∀a. a -> IO a =
  fun @a (x : a) -> coerce @(_ -> IO) (Pair @a @World x)
dict$Monad$IO$ll3 : ∀a b. IO a -> (a -> IO b) -> World -> Pair b World =
  fun @a @b (mx : IO a) (f : a -> IO b) (world0 : World) ->
    match coerce @(IO -> _) mx world0 with
    | Pair x world1 -> coerce @(IO -> _) (f x) world1
dict$Monad$IO$ll4 : ∀a b. IO a -> (a -> IO b) -> IO b =
  fun @a @b (mx : IO a) (f : a -> IO b) ->
    coerce @(_ -> IO) (dict$Monad$IO$ll3 @a @b mx f)
io$ll1 : ∀a b. (a -> b) -> a -> World -> Pair b World =
  fun @a @b (f : a -> b) (x : a) (world : World) ->
    let y : b = f x in
    seq @b @(Pair b World) y (Pair @b @World y world)
io$ll2 : ∀a b. (a -> b) -> a -> IO b =
  fun @a @b (f : a -> b) (x : a) ->
    coerce @(_ -> IO) (io$ll1 @a @b f x)
print$ll1 : Int -> IO Unit = io$ll2 @Int @Unit puti
qsort$ll1 : Int -> Int -> Bool =
  fun (x : Int) (y : Int) -> lt_int y x
qsort$ll2 : List Int -> List Int =
  fun (xs : List Int) ->
    match xs with
    | Nil -> Nil @Int
    | Cons x xs ->
      match partition$ll1 @Int (qsort$ll1 x) xs with
      | Pair ys zs ->
        let dict : Monoid (List Int) =
              let empty : List Int = Nil @Int
              and append : List Int -> List Int -> List Int =
                    dict$Monoid$List$ll1 @Int
              in
              Dict$Monoid @(List Int) empty append
        in
        (match dict with
         | Dict$Monoid _ append ->
           append) (qsort$ll2 ys) (Cons @Int x (qsort$ll2 zs))
main$ll1 : List Int -> IO Unit =
  fun (xs : List Int) ->
    dict$Foldable$List$ll1 @Int @(IO Unit) (traverse_$ll1 @Int @IO dict$Monad$IO print$ll1) (coerce @(_ -> IO) (Pair @Unit @World Unit)) (qsort$ll2 xs)
main$ll2 : Int -> IO Unit =
  fun (n : Int) ->
    let mx : IO (List Int) =
          sequence$ll3 @Int @IO dict$Monad$IO (replicate$ll1 @(IO Int) n input)
    in
    coerce @(_ -> IO) (dict$Monad$IO$ll3 @(List Int) @Unit mx main$ll1)
