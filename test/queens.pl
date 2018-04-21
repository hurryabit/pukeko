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
external eq_int :: Int -> Int -> Bool = "eq"
external lt_int :: Int -> Int -> Bool = "lt"
external le_int :: Int -> Int -> Bool = "le"
external add_int :: Int -> Int -> Int = "add"
external sub_int :: Int -> Int -> Int = "sub"
external seq :: forall a b. a -> b -> b = "seq"
external puti :: Int -> Unit = "puti"
external geti :: Unit -> Int = "geti"
monoidInt :: Monoid Int = .Monoid @Int monoidInt.empty add_int
print :: Int -> IO Unit = io.L2 @Int @Unit puti
input :: IO Int = coerce @(_ -> IO) (io.L1 @Unit @Int geti Unit)
ints :: List Int =
  let rec go :: Int -> List Int = ints.L1 go in
  go 1
solve_aux :: List (List Int) -> List (List Int) =
  \(kss :: List (List Int)) ->
    case kss of
    | Nil -> Cons @(List Int) (Nil @Int) (Nil @(List Int))
    | Cons ks kss ->
      let monoid.m :: Monoid (List (List Int)) =
            .Monoid @(List (List Int)) (monoidList.empty @(List Int)) (monoidList.append.L1 @(List Int))
      and f :: Int -> List (List Int) = solve_aux.L2 kss
      in
      foldableList.foldr.L1 @Int @(List (List Int)) (foldMap.L1 @Int @(List (List Int)) monoid.m f) (case monoid.m of
                                                                                                     | .Monoid empty _ ->
                                                                                                       empty) ks
main :: IO Unit =
  coerce @(_ -> IO) (monadIO.bind.L1 @Int @Unit input main.L1)
monoidInt.empty :: Int = 0
monoidList.empty :: forall a. List a = Nil
foldMap.L1 :: forall a m. Monoid m -> (a -> m) -> a -> m -> m =
  \@a @m (monoid.m :: Monoid m) (f :: a -> m) (x :: a) ->
    (case monoid.m of
     | .Monoid _ append -> append) (f x)
length.L1 :: forall a. a -> Int = \@a (x :: a) -> 1
monoidList.append.L1 :: forall a. List a -> List a -> List a =
  \@a (xs :: List a) (ys :: List a) ->
    foldableList.foldr.L1 @a @(List a) (Cons @a) ys xs
functorList.map.L1 :: forall a b. (a -> b) -> List a -> List b =
  \@a @b (f :: a -> b) (xs :: List a) ->
    case xs of
    | Nil -> Nil @b
    | Cons x xs -> Cons @b (f x) (functorList.map.L1 @a @b f xs)
foldableList.foldr.L1 :: forall a b. (a -> b -> b) -> b -> List a -> b =
  \@a @b (f :: a -> b -> b) (y0 :: b) (xs :: List a) ->
    case xs of
    | Nil -> y0
    | Cons x xs -> f x (foldableList.foldr.L1 @a @b f y0 xs)
take.L1 :: forall a. Int -> List a -> List a =
  \@a (n :: Int) (xs :: List a) ->
    case le_int n 0 of
    | False ->
      case xs of
      | Nil -> Nil @a
      | Cons x xs -> Cons @a x (take.L1 @a (sub_int n 1) xs)
    | True -> Nil @a
replicate.L1 :: forall a. Int -> a -> List a =
  \@a (n :: Int) (x :: a) ->
    case le_int n 0 of
    | False -> Cons @a x (replicate.L1 @a (sub_int n 1) x)
    | True -> Nil @a
zip_of.L1 :: forall a b c. (a -> b -> c) -> List a -> List b -> List c =
  \@a @b @c (f :: a -> b -> c) (xs :: List a) (ys :: List b) ->
    case xs of
    | Nil -> Nil @c
    | Cons x xs ->
      case ys of
      | Nil -> Nil @c
      | Cons y ys -> Cons @c (f x y) (zip_of.L1 @a @b @c f xs ys)
monadIO.bind.L1 :: forall a b. IO a -> (a -> IO b) -> World -> Pair b World =
  \@a @b (mx :: IO a) (f :: a -> IO b) (world0 :: World) ->
    case coerce @(IO -> _) mx world0 of
    | Pair x world1 -> coerce @(IO -> _) (f x) world1
io.L1 :: forall a b. (a -> b) -> a -> World -> Pair b World =
  \@a @b (f :: a -> b) (x :: a) (world :: World) ->
    let y :: b = f x in
    seq @b @(Pair b World) y (Pair @b @World y world)
io.L2 :: forall a b. (a -> b) -> a -> IO b =
  \@a @b (f :: a -> b) (x :: a) ->
    coerce @(_ -> IO) (io.L1 @a @b f x)
diff.L1 :: List Int -> List Int -> List Int =
  \(xs :: List Int) (ys :: List Int) ->
    case xs of
    | Nil -> Nil @Int
    | Cons x xs' ->
      case ys of
      | Nil -> xs
      | Cons y ys' ->
        case lt_int x y of
        | False ->
          case eq_int x y of
          | False -> diff.L1 xs ys'
          | True -> diff.L1 xs' ys'
        | True -> Cons @Int x (diff.L1 xs' ys)
ints.L1 :: (Int -> List Int) -> Int -> List Int =
  \(go :: Int -> List Int) (k :: Int) ->
    Cons @Int k (go (add_int k 1))
solve_aux.L1 :: Int -> List Int -> Int -> List Int =
  \(k :: Int) (ls :: List Int) (i :: Int) ->
    diff.L1 ls (Cons @Int (sub_int k i) (Cons @Int k (Cons @Int (add_int k i) (Nil @Int))))
solve_aux.L2 :: List (List Int) -> Int -> List (List Int) =
  \(kss :: List (List Int)) (k :: Int) ->
    functorList.map.L1 @(List Int) @(List Int) (Cons @Int k) (solve_aux (zip_of.L1 @(List Int) @Int @(List Int) (solve_aux.L1 k) kss ints))
main.L1 :: Int -> IO Unit =
  \(n :: Int) ->
    print (let f :: List Int -> Int = length.L1 @(List Int) in
           foldableList.foldr.L1 @(List Int) @Int (foldMap.L1 @(List Int) @Int monoidInt f) monoidInt.empty (solve_aux (replicate.L1 @(List Int) n (take.L1 @Int n ints))))
