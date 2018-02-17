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
external geti : Unit -> Int = "geti"
external puti : Int -> Unit = "puti"
external seq : ∀a b. a -> b -> b = "seq"
bind$ll1 : ∀m. Monad m -> (∀a b. m a -> (a -> m b) -> m b) =
  fun @m ->
    fun (dict : Monad m) ->
      match dict with
      | Dict$Monad @m _ bind -> bind
dict$Monad$IO : Monad IO =
  let pure : ∀a. a -> IO a = fun @a -> dict$Monad$IO$ll2 @a
  and bind : ∀a b. IO a -> (a -> IO b) -> IO b =
        fun @a @b -> dict$Monad$IO$ll4 @a @b
  in
  Dict$Monad @IO pure bind
dict$Monad$IO$ll1 : ∀a. a -> World -> Pair a World =
  fun @a -> Pair @a @World
dict$Monad$IO$ll2 : ∀a. a -> IO a =
  fun @a -> fun (x : a) -> coerce @(_ -> IO) (dict$Monad$IO$ll1 @a x)
dict$Monad$IO$ll3 : ∀a b. IO a -> (a -> IO b) -> World -> Pair b World =
  fun @a @b ->
    fun (mx : IO a) (f : a -> IO b) (world0 : World) ->
      match coerce @(IO -> _) mx world0 with
      | Pair @a @World x world1 -> coerce @(IO -> _) (f x) world1
dict$Monad$IO$ll4 : ∀a b. IO a -> (a -> IO b) -> IO b =
  fun @a @b ->
    fun (mx : IO a) (f : a -> IO b) ->
      coerce @(_ -> IO) (dict$Monad$IO$ll3 @a @b mx f)
fst$ll1 : ∀a b. Pair a b -> a =
  fun @a @b ->
    fun (p : Pair a b) ->
      match p with
      | Pair @a @b x _ -> x
input : IO Int = io$ll2 @Unit @Int geti Unit
io$ll1 : ∀a b. (a -> b) -> a -> World -> Pair b World =
  fun @a @b ->
    fun (f : a -> b) (x : a) (world : World) ->
      let y : b = f x in
      seq @b @(Pair b World) y (Pair @b @World y world)
io$ll2 : ∀a b. (a -> b) -> a -> IO b =
  fun @a @b ->
    fun (f : a -> b) (x : a) -> coerce @(_ -> IO) (io$ll1 @a @b f x)
main : IO Unit =
  bind$ll1 @IO dict$Monad$IO @Int @Unit input main$ll2
main$ll1 : Int -> Int -> IO Unit =
  fun (x : Int) (y : Int) ->
    let p : Pair Int Int = Pair @Int @Int x y in
    semi$ll2 @Unit @IO dict$Monad$IO (print$ll1 (fst$ll1 @Int @Int p)) (print$ll1 (snd$ll1 @Int @Int p))
main$ll2 : Int -> IO Unit =
  fun (x : Int) ->
    bind$ll1 @IO dict$Monad$IO @Int @Unit input (main$ll1 x)
print$ll1 : Int -> IO Unit =
  fun (n : Int) -> io$ll2 @Int @Unit puti n
semi$ll1 : ∀a m. m a -> Unit -> m a =
  fun @a @m -> fun (m2 : m a) (x : Unit) -> m2
semi$ll2 : ∀a m. Monad m -> m Unit -> m a -> m a =
  fun @a @m ->
    fun (dict$Monad$m : Monad m) (m1 : m Unit) (m2 : m a) ->
      bind$ll1 @m dict$Monad$m @Unit @a m1 (semi$ll1 @a @m m2)
snd$ll1 : ∀a b. Pair a b -> b =
  fun @a @b ->
    fun (p : Pair a b) ->
      match p with
      | Pair @a @b _ y -> y
