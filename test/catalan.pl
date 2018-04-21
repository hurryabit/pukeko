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
external abort :: forall a. a = "abort"
external le_int :: Int -> Int -> Bool = "le"
external add_int :: Int -> Int -> Int = "add"
external sub_int :: Int -> Int -> Int = "sub"
external mul_int :: Int -> Int -> Int = "mul"
external mod :: Int -> Int -> Int = "mod"
external seq :: forall a b. a -> b -> b = "seq"
external puti :: Int -> Unit = "puti"
external geti :: Unit -> Int = "geti"
print :: Int -> IO Unit = io.L2 @Int @Unit puti
input :: IO Int = coerce @(_ -> IO) (io.L1 @Unit @Int geti Unit)
p :: Int = 100000007
sum_p :: List Int -> Int =
  foldableList.foldl.L1 @Int @Int add_p.L1 0
sols :: List Int =
  Cons @Int 1 (functorList.map.L1 @(List Int) @Int sols.L1 (let rec scanl_f :: List Int -> List Int -> List (List Int) =
                                                                      scanl.L1 @Int @(List Int) sols.L2 scanl_f
                                                            in
                                                            scanl_f (Nil @Int) sols))
main :: IO Unit =
  coerce @(_ -> IO) (monadIO.bind.L1 @Int @Unit input main.L1)
functorList.map.L1 :: forall a b. (a -> b) -> List a -> List b =
  fun @a @b (f :: a -> b) (xs :: List a) ->
    match xs with
    | Nil -> Nil @b
    | Cons x xs -> Cons @b (f x) (functorList.map.L1 @a @b f xs)
foldableList.foldl.L1 :: forall a b. (b -> a -> b) -> b -> List a -> b =
  fun @a @b (f :: b -> a -> b) (y0 :: b) (xs :: List a) ->
    match xs with
    | Nil -> y0
    | Cons x xs -> foldableList.foldl.L1 @a @b f (f y0 x) xs
nth_exn.L1 :: forall a. List a -> Int -> a =
  fun @a (xs :: List a) (n :: Int) ->
    match xs with
    | Nil -> abort @a
    | Cons x xs ->
      match le_int n 0 with
      | False -> nth_exn.L1 @a xs (sub_int n 1)
      | True -> x
zip_with.L1 :: forall a b c. (a -> b -> c) -> List a -> List b -> List c =
  fun @a @b @c (f :: a -> b -> c) (xs :: List a) (ys :: List b) ->
    match xs with
    | Nil -> Nil @c
    | Cons x xs ->
      match ys with
      | Nil -> Nil @c
      | Cons y ys -> Cons @c (f x y) (zip_with.L1 @a @b @c f xs ys)
monadIO.bind.L1 :: forall a b. IO a -> (a -> IO b) -> World -> Pair b World =
  fun @a @b (mx :: IO a) (f :: a -> IO b) (world0 :: World) ->
    match coerce @(IO -> _) mx world0 with
    | Pair x world1 -> coerce @(IO -> _) (f x) world1
io.L1 :: forall a b. (a -> b) -> a -> World -> Pair b World =
  fun @a @b (f :: a -> b) (x :: a) (world :: World) ->
    let y :: b = f x in
    seq @b @(Pair b World) y (Pair @b @World y world)
io.L2 :: forall a b. (a -> b) -> a -> IO b =
  fun @a @b (f :: a -> b) (x :: a) ->
    coerce @(_ -> IO) (io.L1 @a @b f x)
mul_p.L1 :: Int -> Int -> Int =
  fun (x :: Int) (y :: Int) -> mod (mul_int x y) p
add_p.L1 :: Int -> Int -> Int =
  fun (x :: Int) (y :: Int) -> mod (add_int x y) p
scanl.L1 :: forall a b. (b -> a -> b) -> (b -> List a -> List b) -> b -> List a -> List b =
  fun @a @b (f :: b -> a -> b) (scanl_f :: b -> List a -> List b) (y0 :: b) (xs :: List a) ->
    match xs with
    | Nil -> Nil @b
    | Cons x xs ->
      let y0 :: b = f y0 x in
      Cons @b y0 (scanl_f y0 xs)
sols.L1 :: List Int -> Int =
  fun (xs :: List Int) ->
    sum_p (zip_with.L1 @Int @Int @Int mul_p.L1 sols xs)
sols.L2 :: List Int -> Int -> List Int =
  fun (xs :: List Int) (x :: Int) -> Cons @Int x xs
main.L1 :: Int -> IO Unit =
  fun (n :: Int) -> print (nth_exn.L1 @Int sols n)
