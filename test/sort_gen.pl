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
       | .Foldable (forall a b. (a -> b -> b) -> b -> t a -> b) (forall a b. (b -> a -> b) -> b -> t a -> b)
data Functor f =
       | .Functor (forall a b. (a -> b) -> f a -> f b)
data List a =
       | Nil
       | Cons a (List a)
data Monad m =
       | .Monad (forall a. a -> m a) (forall a b. m a -> (a -> m b) -> m b)
data World
data IO a = World -> Pair a World
external le_int : Int -> Int -> Bool = "le"
external sub_int : Int -> Int -> Int = "sub"
external mul_int : Int -> Int -> Int = "mul"
external mod : Int -> Int -> Int = "mod"
external seq : forall a b. a -> b -> b = "seq"
external puti : Int -> Unit = "puti"
external geti : Unit -> Int = "geti"
monadIO : Monad IO = .Monad @IO monadIO.pure.L2 monadIO.bind.L2
print : Int -> IO Unit = io.L2 @Int @Unit puti
input : IO Int = coerce @(_ -> IO) (io.L1 @Unit @Int geti Unit)
main : IO Unit =
  coerce @(_ -> IO) (monadIO.bind.L1 @Int @Unit input main.L2)
foldableList.foldr.L1 : forall a b. (a -> b -> b) -> b -> List a -> b =
  fun @a @b (f : a -> b -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> y0
    | Cons x xs -> f x (foldableList.foldr.L1 @a @b f y0 xs)
take.L1 : forall a. Int -> List a -> List a =
  fun @a (n : Int) (xs : List a) ->
    match le_int n 0 with
    | False ->
      match xs with
      | Nil -> Nil @a
      | Cons x xs -> Cons @a x (take.L1 @a (sub_int n 1) xs)
    | True -> Nil @a
semi.L1 : forall a m. m a -> Unit -> m a =
  fun @a @m (m2 : m a) (x : Unit) -> m2
semi.L2 : forall a m. Monad m -> m Unit -> m a -> m a =
  fun @a @m (monad.m : Monad m) (m1 : m Unit) (m2 : m a) ->
    (match monad.m with
     | .Monad _ bind -> bind) @Unit @a m1 (semi.L1 @a @m m2)
traverse_.L1 : forall a m. Monad m -> (a -> m Unit) -> a -> m Unit -> m Unit =
  fun @a @m (monad.m : Monad m) (f : a -> m Unit) (x : a) ->
    semi.L2 @Unit @m monad.m (f x)
monadIO.pure.L2 : forall a. a -> IO a =
  fun @a (x : a) -> coerce @(_ -> IO) (Pair @a @World x)
monadIO.bind.L1 : forall a b. IO a -> (a -> IO b) -> World -> Pair b World =
  fun @a @b (mx : IO a) (f : a -> IO b) (world0 : World) ->
    match coerce @(IO -> _) mx world0 with
    | Pair x world1 -> coerce @(IO -> _) (f x) world1
monadIO.bind.L2 : forall a b. IO a -> (a -> IO b) -> IO b =
  fun @a @b (mx : IO a) (f : a -> IO b) ->
    coerce @(_ -> IO) (monadIO.bind.L1 @a @b mx f)
io.L1 : forall a b. (a -> b) -> a -> World -> Pair b World =
  fun @a @b (f : a -> b) (x : a) (world : World) ->
    let y : b = f x in
    seq @b @(Pair b World) y (Pair @b @World y world)
io.L2 : forall a b. (a -> b) -> a -> IO b =
  fun @a @b (f : a -> b) (x : a) ->
    coerce @(_ -> IO) (io.L1 @a @b f x)
gen.L1 : forall a. (a -> a) -> a -> List a =
  fun @a (f : a -> a) (x : a) -> Cons @a x (gen.L1 @a f (f x))
main.L1 : Int -> Int =
  fun (x : Int) -> mod (mul_int 91 x) 1000000007
main.L2 : Int -> IO Unit =
  fun (n : Int) ->
    let m1 : IO Unit = print n
    and m2 : IO Unit =
          foldableList.foldr.L1 @Int @(IO Unit) (traverse_.L1 @Int @IO monadIO print) (coerce @(_ -> IO) (Pair @Unit @World Unit)) (take.L1 @Int n (gen.L1 @Int main.L1 1))
    in
    let f : Unit -> IO Unit = semi.L1 @Unit @IO m2 in
    coerce @(_ -> IO) (monadIO.bind.L1 @Unit @Unit m1 f)
