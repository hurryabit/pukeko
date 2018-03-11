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
external le_int : Int -> Int -> Bool = "le"
external sub_int : Int -> Int -> Int = "sub"
external mul_int : Int -> Int -> Int = "mul"
external mod : Int -> Int -> Int = "mod"
external seq : ∀a b. a -> b -> b = "seq"
external puti : Int -> Unit = "puti"
external geti : Unit -> Int = "geti"
dict$Monad$IO : Monad IO =
  Dict$Monad @IO dict$Monad$IO$ll2 dict$Monad$IO$ll4
input : IO Int = coerce @(_ -> IO) (io$ll1 @Unit @Int geti Unit)
main : IO Unit =
  coerce @(_ -> IO) (dict$Monad$IO$ll3 @Int @Unit input main$ll2)
dict$Foldable$List$ll1 : ∀a b. (a -> b -> b) -> b -> List a -> b =
  fun @a @b (f : a -> b -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> y0
    | Cons x xs -> f x (dict$Foldable$List$ll1 @a @b f y0 xs)
take$ll1 : ∀a. Int -> List a -> List a =
  fun @a (n : Int) (xs : List a) ->
    match le_int n 0 with
    | False ->
      match xs with
      | Nil -> Nil @a
      | Cons x xs -> Cons @a x (take$ll1 @a (sub_int n 1) xs)
    | True -> Nil @a
semi$ll1 : ∀a m. m a -> Unit -> m a =
  fun @a @m (m2 : m a) (x : Unit) -> m2
semi$ll2 : ∀a m. Monad m -> m Unit -> m a -> m a =
  fun @a @m (dict$Monad$m : Monad m) (m1 : m Unit) (m2 : m a) ->
    (match dict$Monad$m with
     | Dict$Monad _ bind -> bind) @Unit @a m1 (semi$ll1 @a @m m2)
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
gen$ll1 : ∀a. (a -> a) -> a -> List a =
  fun @a (f : a -> a) (x : a) -> Cons @a x (gen$ll1 @a f (f x))
main$ll1 : Int -> Int =
  fun (x : Int) -> mod (mul_int 91 x) 1000000007
main$ll2 : Int -> IO Unit =
  fun (n : Int) ->
    let m1 : IO Unit = print$ll1 n
    and m2 : IO Unit =
          dict$Foldable$List$ll1 @Int @(IO Unit) (traverse_$ll1 @Int @IO dict$Monad$IO print$ll1) (coerce @(_ -> IO) (Pair @Unit @World Unit)) (take$ll1 @Int n (gen$ll1 @Int main$ll1 1))
    in
    let f : Unit -> IO Unit = semi$ll1 @Unit @IO m2 in
    coerce @(_ -> IO) (dict$Monad$IO$ll3 @Unit @Unit m1 f)
