data Bool =
       | False
       | True
data Char
data Choice a b =
       | First a
       | Second b
data Dict$Eq a =
       | Dict$Eq (a -> a -> Bool)
data Dict$Foldable t =
       | Dict$Foldable (∀a b. (a -> b -> b) -> b -> t a -> b) (∀a b. (b -> a -> b) -> b -> t a -> b)
data Dict$Functor f =
       | Dict$Functor (∀a b. (a -> b) -> f a -> f b)
data Dict$Monad m =
       | Dict$Monad (∀a. a -> m a) (∀a b. m a -> (a -> m b) -> m b)
data Dict$Monoid m =
       | Dict$Monoid m (m -> m -> m)
data Dict$Ord a =
       | Dict$Ord (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool)
data Dict$Ring a =
       | Dict$Ring (a -> a) (a -> a -> a) (a -> a -> a) (a -> a -> a)
data IO a = World -> Pair a World
data Int
data List a =
       | Nil
       | Cons a (List a)
data Option a =
       | None
       | Some a
data Pair a b =
       | Pair a b
data Unit =
       | Unit
data World =
       | World
external add_int : Int -> Int -> Int = "add"
external ge_int : Int -> Int -> Bool = "ge"
external geti : Unit -> Int = "geti"
external gt_int : Int -> Int -> Bool = "gt"
external le_int : Int -> Int -> Bool = "le"
external lt_int : Int -> Int -> Bool = "lt"
external mul_int : Int -> Int -> Int = "mul"
external neg_int : Int -> Int = "neg"
external puti : Int -> Unit = "puti"
external seq : ∀a b. a -> b -> b = "seq"
external sub_int : Int -> Int -> Int = "sub"
bind : ∀m. Dict$Monad m -> (∀a b. m a -> (a -> m b) -> m b) =
  fun @m ->
    fun (dict : Dict$Monad m) ->
      match dict with
      | Dict$Monad @m _ bind -> bind
count_down : Int -> IO Unit =
  fun (k : Int) ->
    when @IO dict$Monad$IO (ge @Int dict$Ord$Int k 0) (semi @Unit @IO dict$Monad$IO (print k) (count_down (sub @Int dict$Ring$Int k 1)))
dict$Monad$IO : Dict$Monad IO =
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
dict$Ord$Int : Dict$Ord Int =
  let ge : Int -> Int -> Bool = ge_int
  and gt : Int -> Int -> Bool = gt_int
  and le : Int -> Int -> Bool = le_int
  and lt : Int -> Int -> Bool = lt_int
  in
  Dict$Ord @Int ge gt le lt
dict$Ring$Int : Dict$Ring Int =
  let neg : Int -> Int = neg_int
  and add : Int -> Int -> Int = add_int
  and sub : Int -> Int -> Int = sub_int
  and mul : Int -> Int -> Int = mul_int
  in
  Dict$Ring @Int neg add sub mul
ge : ∀a. Dict$Ord a -> a -> a -> Bool =
  fun @a ->
    fun (dict : Dict$Ord a) ->
      match dict with
      | Dict$Ord @a ge _ _ _ -> ge
gt : ∀a. Dict$Ord a -> a -> a -> Bool =
  fun @a ->
    fun (dict : Dict$Ord a) ->
      match dict with
      | Dict$Ord @a _ gt _ _ -> gt
input : IO Int = io @Unit @Int geti Unit
io : ∀a b. (a -> b) -> a -> IO b =
  fun @a @b ->
    fun (f : a -> b) (x : a) -> coerce @(_ -> IO) (io$ll1 @a @b f x)
io$ll1 : ∀a b. (a -> b) -> a -> World -> Pair b World =
  fun @a @b ->
    fun (f : a -> b) (x : a) (world : World) ->
      let y : b = f x in
      seq @b @(Pair b World) y (Pair @b @World y world)
main : IO Unit = bind @IO dict$Monad$IO @Int @Unit input main$ll2
main$ll1 : Int -> Int -> IO Unit =
  fun (k : Int) (n : Int) ->
    repeat @IO dict$Monad$IO k (count_down n)
main$ll2 : Int -> IO Unit =
  fun (k : Int) ->
    bind @IO dict$Monad$IO @Int @Unit input (main$ll1 k)
print : Int -> IO Unit = fun (n : Int) -> io @Int @Unit puti n
pure : ∀m. Dict$Monad m -> (∀a. a -> m a) =
  fun @m ->
    fun (dict : Dict$Monad m) ->
      match dict with
      | Dict$Monad @m pure _ -> pure
repeat : ∀m. Dict$Monad m -> Int -> m Unit -> m Unit =
  fun @m ->
    fun (dict$Monad$m : Dict$Monad m) (k : Int) (m : m Unit) ->
      when @m dict$Monad$m (gt @Int dict$Ord$Int k 0) (semi @Unit @m dict$Monad$m m (repeat @m dict$Monad$m (sub @Int dict$Ring$Int k 1) m))
semi : ∀a m. Dict$Monad m -> m Unit -> m a -> m a =
  fun @a @m ->
    fun (dict$Monad$m : Dict$Monad m) (m1 : m Unit) (m2 : m a) ->
      bind @m dict$Monad$m @Unit @a m1 (semi$ll1 @a @m m2)
semi$ll1 : ∀a m. m a -> Unit -> m a =
  fun @a @m -> fun (m2 : m a) (x : Unit) -> m2
sub : ∀a. Dict$Ring a -> a -> a -> a =
  fun @a ->
    fun (dict : Dict$Ring a) ->
      match dict with
      | Dict$Ring @a _ _ sub _ -> sub
when : ∀m. Dict$Monad m -> Bool -> m Unit -> m Unit =
  fun @m ->
    fun (dict$Monad$m : Dict$Monad m) (p : Bool) (m : m Unit) ->
      match p with
      | False -> pure @m dict$Monad$m @Unit Unit
      | True -> m
