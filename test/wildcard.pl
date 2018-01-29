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
data Dict$Eq a =
       | Dict$Eq (a -> a -> Bool)
data Dict$Ord a =
       | Dict$Ord (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool)
data Dict$Monoid m =
       | Dict$Monoid m (m -> m -> m)
data Dict$Ring a =
       | Dict$Ring (a -> a) (a -> a -> a) (a -> a -> a) (a -> a -> a)
data Int
data Char
data Dict$Foldable t =
       | Dict$Foldable (∀a b. (a -> b -> b) -> b -> t a -> b) (∀a b. (b -> a -> b) -> b -> t a -> b)
data Dict$Functor f =
       | Dict$Functor (∀a b. (a -> b) -> f a -> f b)
data List a =
       | Nil
       | Cons a (List a)
data Dict$Monad m =
       | Dict$Monad (∀a. a -> m a) (∀a b. m a -> (a -> m b) -> m b)
(>>=) : ∀m. Dict$Monad m -> (∀a b. m a -> (a -> m b) -> m b) =
  fun @m ->
    fun (dict : Dict$Monad m) ->
      match dict with
      | Dict$Monad @m pure (>>=) -> (>>=)
(;ll1) : ∀a m. m a -> Unit -> m a =
  fun @a @m -> fun (m2 : m a) (x : Unit) -> m2
(;) : ∀a m. Dict$Monad m -> m Unit -> m a -> m a =
  fun @a @m ->
    fun (dict$Monad$m : Dict$Monad m) (m1 : m Unit) (m2 : m a) ->
      (>>=) @m dict$Monad$m @Unit @a m1 ((;ll1) @a @m m2)
data IO a
external pure_io : ∀a. a -> IO a = "return"
external bind_io : ∀a b. IO a -> (a -> IO b) -> IO b = "bind"
dict$Monad$IO : Dict$Monad IO =
  let pure : ∀a. a -> IO a = fun @a -> pure_io @a
  and (>>=) : ∀a b. IO a -> (a -> IO b) -> IO b =
        fun @a @b -> bind_io @a @b
  in
  Dict$Monad @IO pure (>>=)
external print : Int -> IO Unit = "print"
external input : IO Int = "input"
fst : ∀a b. Pair a b -> a =
  fun @a @b ->
    fun (p : Pair a b) ->
      match p with
      | Pair @a @b x fst$pm1 -> x
snd : ∀a b. Pair a b -> b =
  fun @a @b ->
    fun (p : Pair a b) ->
      match p with
      | Pair @a @b snd$pm1 y -> y
main$ll1 : Int -> Int -> IO Unit =
  fun (x : Int) (y : Int) ->
    let p : Pair Int Int = Pair @Int @Int x y in
    (;) @Unit @IO dict$Monad$IO (print (fst @Int @Int p)) (print (snd @Int @Int p))
main$ll2 : Int -> IO Unit =
  fun (x : Int) ->
    (>>=) @IO dict$Monad$IO @Int @Unit input (main$ll1 x)
main : IO Unit = (>>=) @IO dict$Monad$IO @Int @Unit input main$ll2
