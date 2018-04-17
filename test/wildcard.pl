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
external seq : forall a b. a -> b -> b = "seq"
external puti : Int -> Unit = "puti"
external geti : Unit -> Int = "geti"
print : Int -> IO Unit = io.L2 @Int @Unit puti
input : IO Int = coerce @(_ -> IO) (io.L1 @Unit @Int geti Unit)
main : IO Unit =
  coerce @(_ -> IO) (monadIO.bind.L1 @Int @Unit input main.L2)
semi.L1 : forall a m. m a -> Unit -> m a =
  fun @a @m (m2 : m a) (x : Unit) -> m2
monadIO.bind.L1 : forall a b. IO a -> (a -> IO b) -> World -> Pair b World =
  fun @a @b (mx : IO a) (f : a -> IO b) (world0 : World) ->
    match coerce @(IO -> _) mx world0 with
    | Pair x world1 -> coerce @(IO -> _) (f x) world1
io.L1 : forall a b. (a -> b) -> a -> World -> Pair b World =
  fun @a @b (f : a -> b) (x : a) (world : World) ->
    let y : b = f x in
    seq @b @(Pair b World) y (Pair @b @World y world)
io.L2 : forall a b. (a -> b) -> a -> IO b =
  fun @a @b (f : a -> b) (x : a) ->
    coerce @(_ -> IO) (io.L1 @a @b f x)
main.L1 : Int -> Int -> IO Unit =
  fun (x : Int) (y : Int) ->
    let p : Pair Int Int = Pair @Int @Int x y in
    let m1 : IO Unit =
          print (match p with
                 | Pair x _ -> x)
    and m2 : IO Unit =
          print (match p with
                 | Pair _ y -> y)
    in
    let f : Unit -> IO Unit = semi.L1 @Unit @IO m2 in
    coerce @(_ -> IO) (monadIO.bind.L1 @Unit @Unit m1 f)
main.L2 : Int -> IO Unit =
  fun (x : Int) ->
    let f : Int -> IO Unit = main.L1 x in
    coerce @(_ -> IO) (monadIO.bind.L1 @Int @Unit input f)
