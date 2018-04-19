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
external lt_int : Int -> Int -> Bool = "lt"
external le_int : Int -> Int -> Bool = "le"
external sub_int : Int -> Int -> Int = "sub"
external mul_int : Int -> Int -> Int = "mul"
external mod : Int -> Int -> Int = "mod"
external seq : forall a b. a -> b -> b = "seq"
external puti : Int -> Unit = "puti"
functorIO : Functor IO = .Functor @IO functorIO.map.L2
monadIO : Monad IO =
  .Monad @IO functorIO monadIO.pure.L2 monadIO.bind.L2
print : Int -> IO Unit = io.L2 @Int @Unit puti
random : List Int = gen.L1 @Int random.L1 1
main : IO Unit =
  let m1 : IO Unit = print 400000
  and m2 : IO Unit =
        let m1 : IO Unit = print 100000
        and m2 : IO Unit =
              match split_at.L1 @Int 400000 random with
              | Pair xs random ->
                let m1 : IO Unit =
                      foldableList.foldr.L1 @Int @(IO Unit) (traverse_.L1 @Int @IO monadIO print) (coerce @(_ -> IO) (Pair @Unit @World Unit)) xs
                and m2 : IO Unit =
                      match split_at.L1 @Int 100000 random with
                      | Pair ys random ->
                        let zs : List Int = take.L1 @Int 100000 random in
                        let mx : IO (List Unit) =
                              sequence.L3 @Unit @IO monadIO (zip_with.L1 @Int @Int @(IO Unit) (main.L1 400000) ys zs)
                        in
                        coerce @(_ -> IO) (monadIO.bind.L1 @(List Unit) @Unit mx main.L2)
                in
                let f : Unit -> IO Unit = semi.L1 @Unit @IO m2 in
                coerce @(_ -> IO) (monadIO.bind.L1 @Unit @Unit m1 f)
        in
        let f : Unit -> IO Unit = semi.L1 @Unit @IO m2 in
        coerce @(_ -> IO) (monadIO.bind.L1 @Unit @Unit m1 f)
  in
  let f : Unit -> IO Unit = semi.L1 @Unit @IO m2 in
  coerce @(_ -> IO) (monadIO.bind.L1 @Unit @Unit m1 f)
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
zip_with.L1 : forall a b c. (a -> b -> c) -> List a -> List b -> List c =
  fun @a @b @c (f : a -> b -> c) (xs : List a) (ys : List b) ->
    match xs with
    | Nil -> Nil @c
    | Cons x xs ->
      match ys with
      | Nil -> Nil @c
      | Cons y ys -> Cons @c (f x y) (zip_with.L1 @a @b @c f xs ys)
semi.L1 : forall a m. m a -> Unit -> m a =
  fun @a @m (m2 : m a) (x : Unit) -> m2
semi.L2 : forall a m. Monad m -> m Unit -> m a -> m a =
  fun @a @m (monad.m : Monad m) (m1 : m Unit) (m2 : m a) ->
    (match monad.m with
     | .Monad _ _ bind -> bind) @Unit @a m1 (semi.L1 @a @m m2)
sequence.L1 : forall a m. Monad m -> a -> List a -> m (List a) =
  fun @a @m (monad.m : Monad m) (x : a) (xs : List a) ->
    (match monad.m with
     | .Monad _ pure _ -> pure) @(List a) (Cons @a x xs)
sequence.L2 : forall a m. Monad m -> List (m a) -> a -> m (List a) =
  fun @a @m (monad.m : Monad m) (ms : List (m a)) (x : a) ->
    (match monad.m with
     | .Monad _ _ bind ->
       bind) @(List a) @(List a) (sequence.L3 @a @m monad.m ms) (sequence.L1 @a @m monad.m x)
sequence.L3 : forall a m. Monad m -> List (m a) -> m (List a) =
  fun @a @m (monad.m : Monad m) (ms : List (m a)) ->
    match ms with
    | Nil ->
      (match monad.m with
       | .Monad _ pure _ -> pure) @(List a) (Nil @a)
    | Cons m ms ->
      (match monad.m with
       | .Monad _ _ bind ->
         bind) @a @(List a) m (sequence.L2 @a @m monad.m ms)
traverse_.L1 : forall a m. Monad m -> (a -> m Unit) -> a -> m Unit -> m Unit =
  fun @a @m (monad.m : Monad m) (f : a -> m Unit) (x : a) ->
    semi.L2 @Unit @m monad.m (f x)
functorIO.map.L1 : forall a b. (a -> b) -> IO a -> World -> Pair b World =
  fun @a @b (f : a -> b) (mx : IO a) (world0 : World) ->
    match coerce @(IO -> _) mx world0 with
    | Pair x world1 -> Pair @b @World (f x) world1
functorIO.map.L2 : forall a b. (a -> b) -> IO a -> IO b =
  fun @a @b (f : a -> b) (mx : IO a) ->
    coerce @(_ -> IO) (functorIO.map.L1 @a @b f mx)
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
split_at.L1 : forall a. Int -> List a -> Pair (List a) (List a) =
  fun @a (n : Int) (xs : List a) ->
    match le_int n 0 with
    | False ->
      match xs with
      | Nil -> Pair @(List a) @(List a) (Nil @a) (Nil @a)
      | Cons x xs ->
        match split_at.L1 @a (sub_int n 1) xs with
        | Pair ys zs -> Pair @(List a) @(List a) (Cons @a x ys) zs
    | True -> Pair @(List a) @(List a) (Nil @a) xs
random.L1 : Int -> Int =
  fun (x : Int) -> mod (mul_int 91 x) 1000000007
main.L1 : Int -> Int -> Int -> IO Unit =
  fun (n : Int) (y : Int) (z : Int) ->
    let y : Int = mod y n in
    let z : Int = mod z n in
    match lt_int y z with
    | False ->
      let m1 : IO Unit = print z
      and m2 : IO Unit = print y
      in
      let f : Unit -> IO Unit = semi.L1 @Unit @IO m2 in
      coerce @(_ -> IO) (monadIO.bind.L1 @Unit @Unit m1 f)
    | True ->
      let m1 : IO Unit = print y
      and m2 : IO Unit = print z
      in
      let f : Unit -> IO Unit = semi.L1 @Unit @IO m2 in
      coerce @(_ -> IO) (monadIO.bind.L1 @Unit @Unit m1 f)
main.L2 : List Unit -> IO Unit =
  fun (x : List Unit) -> coerce @(_ -> IO) (Pair @Unit @World Unit)
