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
data RmqTree a =
       | RmqEmpty
       | RmqNode Int Int a (RmqTree a) (RmqTree a)
external abort :: forall a. a = "abort"
external lt_int :: Int -> Int -> Bool = "lt"
external le_int :: Int -> Int -> Bool = "le"
external gt_int :: Int -> Int -> Bool = "gt"
external add_int :: Int -> Int -> Int = "add"
external sub_int :: Int -> Int -> Int = "sub"
external seq :: forall a b. a -> b -> b = "seq"
external puti :: Int -> Unit = "puti"
external geti :: Unit -> Int = "geti"
functorIO :: Functor IO = .Functor @IO functorIO.map.L2
monadIO :: Monad IO =
  .Monad @IO functorIO monadIO.pure.L2 monadIO.bind.L2
print :: Int -> IO Unit = io.L2 @Int @Unit puti
input :: IO Int = coerce @(_ -> IO) (io.L1 @Unit @Int geti Unit)
nats :: List Int =
  let rec nats_from :: Int -> List Int = nats.L1 nats_from in
  nats_from 0
infinity :: Int = 1000000000
main :: IO Unit =
  coerce @(_ -> IO) (monadIO.bind.L1 @Int @Unit input main.L6)
replicate.L1 :: forall a. Int -> a -> List a =
  \@a (n :: Int) (x :: a) ->
    case le_int n 0 of
    | False -> Cons @a x (replicate.L1 @a (sub_int n 1) x)
    | True -> Nil @a
zip_with.L1 :: forall a b c. (a -> b -> c) -> List a -> List b -> List c =
  \@a @b @c (f :: a -> b -> c) (xs :: List a) (ys :: List b) ->
    case xs of
    | Nil -> Nil @c
    | Cons x xs ->
      case ys of
      | Nil -> Nil @c
      | Cons y ys -> Cons @c (f x y) (zip_with.L1 @a @b @c f xs ys)
sequence.L1 :: forall a m. Monad m -> a -> List a -> m (List a) =
  \@a @m (monad.m :: Monad m) (x :: a) (xs :: List a) ->
    (case monad.m of
     | .Monad _ pure _ -> pure) @(List a) (Cons @a x xs)
sequence.L2 :: forall a m. Monad m -> List (m a) -> a -> m (List a) =
  \@a @m (monad.m :: Monad m) (ms :: List (m a)) (x :: a) ->
    (case monad.m of
     | .Monad _ _ bind ->
       bind) @(List a) @(List a) (sequence.L3 @a @m monad.m ms) (sequence.L1 @a @m monad.m x)
sequence.L3 :: forall a m. Monad m -> List (m a) -> m (List a) =
  \@a @m (monad.m :: Monad m) (ms :: List (m a)) ->
    case ms of
    | Nil ->
      (case monad.m of
       | .Monad _ pure _ -> pure) @(List a) (Nil @a)
    | Cons m ms ->
      (case monad.m of
       | .Monad _ _ bind ->
         bind) @a @(List a) m (sequence.L2 @a @m monad.m ms)
functorIO.map.L1 :: forall a b. (a -> b) -> IO a -> World -> Pair b World =
  \@a @b (f :: a -> b) (mx :: IO a) (world0 :: World) ->
    case coerce @(IO -> _) mx world0 of
    | Pair x world1 -> Pair @b @World (f x) world1
functorIO.map.L2 :: forall a b. (a -> b) -> IO a -> IO b =
  \@a @b (f :: a -> b) (mx :: IO a) ->
    coerce @(_ -> IO) (functorIO.map.L1 @a @b f mx)
monadIO.pure.L2 :: forall a. a -> IO a =
  \@a (x :: a) -> coerce @(_ -> IO) (Pair @a @World x)
monadIO.bind.L1 :: forall a b. IO a -> (a -> IO b) -> World -> Pair b World =
  \@a @b (mx :: IO a) (f :: a -> IO b) (world0 :: World) ->
    case coerce @(IO -> _) mx world0 of
    | Pair x world1 -> coerce @(IO -> _) (f x) world1
monadIO.bind.L2 :: forall a b. IO a -> (a -> IO b) -> IO b =
  \@a @b (mx :: IO a) (f :: a -> IO b) ->
    coerce @(_ -> IO) (monadIO.bind.L1 @a @b mx f)
io.L1 :: forall a b. (a -> b) -> a -> World -> Pair b World =
  \@a @b (f :: a -> b) (x :: a) (world :: World) ->
    let y :: b = f x in
    seq @b @(Pair b World) y (Pair @b @World y world)
io.L2 :: forall a b. (a -> b) -> a -> IO b =
  \@a @b (f :: a -> b) (x :: a) ->
    coerce @(_ -> IO) (io.L1 @a @b f x)
nats.L1 :: (Int -> List Int) -> Int -> List Int =
  \(nats_from :: Int -> List Int) (n :: Int) ->
    Cons @Int n (nats_from (add_int n 1))
pair.L1 :: forall a. (a -> a -> a) -> List a -> List a =
  \@a (op :: a -> a -> a) (xs1 :: List a) ->
    case xs1 of
    | Nil -> Nil @a
    | Cons pm$1 pm$2 ->
      case pm$2 of
      | Nil -> xs1
      | Cons x2 xs3 -> Cons @a (op pm$1 x2) (pair.L1 @a op xs3)
single.L1 :: forall a. Int -> a -> RmqTree a =
  \@a (i :: Int) (x :: a) ->
    RmqNode @a i i x (RmqEmpty @a) (RmqEmpty @a)
combine.L1 :: forall a. (a -> a -> a) -> RmqTree a -> RmqTree a -> RmqTree a =
  \@a (op :: a -> a -> a) (t1 :: RmqTree a) (t2 :: RmqTree a) ->
    case t1 of
    | RmqEmpty -> abort @(RmqTree a)
    | RmqNode s1 _ v1 _ _ ->
      case t2 of
      | RmqEmpty -> abort @(RmqTree a)
      | RmqNode _ e2 v2 _ _ -> RmqNode @a s1 e2 (op v1 v2) t1 t2
build.L1 :: forall a. (a -> a -> a) -> (List (RmqTree a) -> RmqTree a) -> List (RmqTree a) -> RmqTree a =
  \@a (op :: a -> a -> a) (run :: List (RmqTree a) -> RmqTree a) (ts :: List (RmqTree a)) ->
    case ts of
    | Nil -> abort @(RmqTree a)
    | Cons pm$1 pm$2 ->
      case pm$2 of
      | Nil -> pm$1
      | Cons _ _ -> run (pair.L1 @(RmqTree a) (combine.L1 @a op) ts)
query.L1 :: forall a. a -> (a -> a -> a) -> Int -> Int -> (RmqTree a -> a) -> RmqTree a -> a =
  \@a (one :: a) (op :: a -> a -> a) (q_lo :: Int) (q_hi :: Int) (aux :: RmqTree a -> a) (t :: RmqTree a) ->
    case t of
    | RmqEmpty -> one
    | RmqNode t_lo t_hi value left right ->
      case let x :: Bool = lt_int q_hi t_lo
           and y :: Bool = gt_int q_lo t_hi
           in
           case x of
           | False -> y
           | True -> True of
      | False ->
        case let x :: Bool = le_int q_lo t_lo
             and y :: Bool = le_int t_hi q_hi
             in
             case x of
             | False -> False
             | True -> y of
        | False -> op (aux left) (aux right)
        | True -> value
      | True -> one
min.L1 :: Int -> Int -> Int =
  \(x :: Int) (y :: Int) ->
    case le_int x y of
    | False -> y
    | True -> x
main.L1 :: RmqTree Int -> Int -> Int -> IO Unit =
  \(t :: RmqTree Int) (lo :: Int) (hi :: Int) ->
    let res :: Int =
          let rec aux :: RmqTree Int -> Int =
                    query.L1 @Int infinity min.L1 lo hi aux
          in
          aux t
    in
    print res
main.L2 :: RmqTree Int -> Int -> IO Unit =
  \(t :: RmqTree Int) (lo :: Int) ->
    let f :: Int -> IO Unit = main.L1 t lo in
    coerce @(_ -> IO) (monadIO.bind.L1 @Int @Unit input f)
main.L3 :: List Unit -> IO Unit =
  \(x :: List Unit) -> coerce @(_ -> IO) (Pair @Unit @World Unit)
main.L4 :: Int -> List Int -> IO Unit =
  \(m :: Int) (xs :: List Int) ->
    let t :: RmqTree Int =
          let rec run :: List (RmqTree Int) -> RmqTree Int =
                    build.L1 @Int min.L1 run
          in
          run (zip_with.L1 @Int @Int @(RmqTree Int) (single.L1 @Int) nats xs)
    in
    let mx :: IO (List Unit) =
          let act :: IO Unit =
                let f :: Int -> IO Unit = main.L2 t in
                coerce @(_ -> IO) (monadIO.bind.L1 @Int @Unit input f)
          in
          sequence.L3 @Unit @IO monadIO (replicate.L1 @(IO Unit) m act)
    in
    coerce @(_ -> IO) (monadIO.bind.L1 @(List Unit) @Unit mx main.L3)
main.L5 :: Int -> Int -> IO Unit =
  \(n :: Int) (m :: Int) ->
    let mx :: IO (List Int) =
          sequence.L3 @Int @IO monadIO (replicate.L1 @(IO Int) n input)
    and f :: List Int -> IO Unit = main.L4 m
    in
    coerce @(_ -> IO) (monadIO.bind.L1 @(List Int) @Unit mx f)
main.L6 :: Int -> IO Unit =
  \(n :: Int) ->
    let f :: Int -> IO Unit = main.L5 n in
    coerce @(_ -> IO) (monadIO.bind.L1 @Int @Unit input f)
