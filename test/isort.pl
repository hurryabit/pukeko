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
external le_int : Int -> Int -> Bool = "le"
external sub_int : Int -> Int -> Int = "sub"
external seq : ∀a b. a -> b -> b = "seq"
external puti : Int -> Unit = "puti"
external geti : Unit -> Int = "geti"
foldableList : Foldable List =
  .Foldable @List foldableList.foldr.L1 foldableList.foldl.L1
monadIO : Monad IO = .Monad @IO monadIO.pure.L2 monadIO.bind.L2
print : Int -> IO Unit = io.L2 @Int @Unit puti
input : IO Int = coerce @(_ -> IO) (io.L1 @Unit @Int geti Unit)
isort : List Int -> List Int =
  fun (xs : List Int) ->
    match xs with
    | Nil -> Nil @Int
    | Cons x xs -> insert.L1 x (isort xs)
main : IO Unit =
  coerce @(_ -> IO) (monadIO.bind.L1 @Int @Unit input main.L2)
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
replicate.L1 : ∀a. Int -> a -> List a =
  fun @a (n : Int) (x : a) ->
    match le_int n 0 with
    | False -> Cons @a x (replicate.L1 @a (sub_int n 1) x)
    | True -> Nil @a
semi.L1 : ∀a m. m a -> Unit -> m a =
  fun @a @m (m2 : m a) (x : Unit) -> m2
semi.L2 : ∀a m. Monad m -> m Unit -> m a -> m a =
  fun @a @m (monad.m : Monad m) (m1 : m Unit) (m2 : m a) ->
    (match monad.m with
     | .Monad _ bind -> bind) @Unit @a m1 (semi.L1 @a @m m2)
sequence.L1 : ∀a m. Monad m -> a -> List a -> m (List a) =
  fun @a @m (monad.m : Monad m) (x : a) (xs : List a) ->
    (match monad.m with
     | .Monad pure _ -> pure) @(List a) (Cons @a x xs)
sequence.L2 : ∀a m. Monad m -> List (m a) -> a -> m (List a) =
  fun @a @m (monad.m : Monad m) (ms : List (m a)) (x : a) ->
    (match monad.m with
     | .Monad _ bind ->
       bind) @(List a) @(List a) (sequence.L3 @a @m monad.m ms) (sequence.L1 @a @m monad.m x)
sequence.L3 : ∀a m. Monad m -> List (m a) -> m (List a) =
  fun @a @m (monad.m : Monad m) (ms : List (m a)) ->
    match ms with
    | Nil ->
      (match monad.m with
       | .Monad pure _ -> pure) @(List a) (Nil @a)
    | Cons m ms ->
      (match monad.m with
       | .Monad _ bind ->
         bind) @a @(List a) m (sequence.L2 @a @m monad.m ms)
traverse_.L1 : ∀a m. Monad m -> (a -> m Unit) -> a -> m Unit -> m Unit =
  fun @a @m (monad.m : Monad m) (f : a -> m Unit) (x : a) ->
    semi.L2 @Unit @m monad.m (f x)
monadIO.pure.L2 : ∀a. a -> IO a =
  fun @a (x : a) -> coerce @(_ -> IO) (Pair @a @World x)
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
insert.L1 : Int -> List Int -> List Int =
  fun (y : Int) (xs : List Int) ->
    match xs with
    | Nil -> Cons @Int y (Nil @Int)
    | Cons x xs' ->
      match le_int y x with
      | False -> Cons @Int x (insert.L1 y xs')
      | True -> Cons @Int y xs
main.L1 : List Int -> IO Unit =
  fun (xs : List Int) ->
    foldableList.foldr.L1 @Int @(IO Unit) (traverse_.L1 @Int @IO monadIO print) (coerce @(_ -> IO) (Pair @Unit @World Unit)) (isort xs)
main.L2 : Int -> IO Unit =
  fun (n : Int) ->
    let mx : IO (List Int) =
          sequence.L3 @Int @IO monadIO (replicate.L1 @(IO Int) n input)
    in
    coerce @(_ -> IO) (monadIO.bind.L1 @(List Int) @Unit mx main.L1)
