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
       | .Ord (Eq a) (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool)
data Monoid m =
       | .Monoid m (m -> m -> m)
data Ring a =
       | .Ring (a -> a) (a -> a -> a) (a -> a -> a) (a -> a -> a)
data Char
data Functor f =
       | .Functor (forall a b. (a -> b) -> f a -> f b)
data Foldable t =
       | .Foldable (forall a b. (a -> b -> b) -> b -> t a -> b) (forall a b. (b -> a -> b) -> b -> t a -> b)
data List a =
       | Nil
       | Cons a (List a)
data Monad m =
       | .Monad (Functor m) (forall a. a -> m a) (forall a b. m a -> (a -> m b) -> m b)
data World
data IO a = World -> Pair a World
external ge_int :: Int -> Int -> Bool = "ge"
external gt_int :: Int -> Int -> Bool = "gt"
external sub_int :: Int -> Int -> Int = "sub"
external seq :: forall a b. a -> b -> b = "seq"
external puti :: Int -> Unit = "puti"
external geti :: Unit -> Int = "geti"
functorIO :: Functor IO = .Functor @IO functorIO.map.L2
monadIO :: Monad IO =
  .Monad @IO functorIO monadIO.pure.L2 monadIO.bind.L2
print :: Int -> IO Unit = io.L2 @Int @Unit puti
input :: IO Int = coerce @(_ -> IO) (io.L1 @Unit @Int geti Unit)
count_down :: Int -> IO Unit =
  fun (k :: Int) ->
    let p :: Bool = ge_int k 0
    and m :: IO Unit =
          let m1 :: IO Unit = print k
          and m2 :: IO Unit = count_down (sub_int k 1)
          in
          let f :: Unit -> IO Unit = semi.L1 @Unit @IO m2 in
          coerce @(_ -> IO) (monadIO.bind.L1 @Unit @Unit m1 f)
    in
    match p with
    | False -> coerce @(_ -> IO) (Pair @Unit @World Unit)
    | True -> m
main :: IO Unit =
  coerce @(_ -> IO) (monadIO.bind.L1 @Int @Unit input main.L2)
semi.L1 :: forall a m. m a -> Unit -> m a =
  fun @a @m (m2 :: m a) (x :: Unit) -> m2
functorIO.map.L1 :: forall a b. (a -> b) -> IO a -> World -> Pair b World =
  fun @a @b (f :: a -> b) (mx :: IO a) (world0 :: World) ->
    match coerce @(IO -> _) mx world0 with
    | Pair x world1 -> Pair @b @World (f x) world1
functorIO.map.L2 :: forall a b. (a -> b) -> IO a -> IO b =
  fun @a @b (f :: a -> b) (mx :: IO a) ->
    coerce @(_ -> IO) (functorIO.map.L1 @a @b f mx)
monadIO.pure.L2 :: forall a. a -> IO a =
  fun @a (x :: a) -> coerce @(_ -> IO) (Pair @a @World x)
monadIO.bind.L1 :: forall a b. IO a -> (a -> IO b) -> World -> Pair b World =
  fun @a @b (mx :: IO a) (f :: a -> IO b) (world0 :: World) ->
    match coerce @(IO -> _) mx world0 with
    | Pair x world1 -> coerce @(IO -> _) (f x) world1
monadIO.bind.L2 :: forall a b. IO a -> (a -> IO b) -> IO b =
  fun @a @b (mx :: IO a) (f :: a -> IO b) ->
    coerce @(_ -> IO) (monadIO.bind.L1 @a @b mx f)
io.L1 :: forall a b. (a -> b) -> a -> World -> Pair b World =
  fun @a @b (f :: a -> b) (x :: a) (world :: World) ->
    let y :: b = f x in
    seq @b @(Pair b World) y (Pair @b @World y world)
io.L2 :: forall a b. (a -> b) -> a -> IO b =
  fun @a @b (f :: a -> b) (x :: a) ->
    coerce @(_ -> IO) (io.L1 @a @b f x)
repeat.L1 :: forall m. Monad m -> Int -> m Unit -> m Unit =
  fun @m (monad.m :: Monad m) (k :: Int) (m :: m Unit) ->
    let p :: Bool = gt_int k 0
    and m :: m Unit =
          let m2 :: m Unit = repeat.L1 @m monad.m (sub_int k 1) m in
          (match monad.m with
           | .Monad _ _ bind -> bind) @Unit @Unit m (semi.L1 @Unit @m m2)
    in
    match p with
    | False ->
      (match monad.m with
       | .Monad _ pure _ -> pure) @Unit Unit
    | True -> m
main.L1 :: Int -> Int -> IO Unit =
  fun (k :: Int) (n :: Int) -> repeat.L1 @IO monadIO k (count_down n)
main.L2 :: Int -> IO Unit =
  fun (k :: Int) ->
    let f :: Int -> IO Unit = main.L1 k in
    coerce @(_ -> IO) (monadIO.bind.L1 @Int @Unit input f)
