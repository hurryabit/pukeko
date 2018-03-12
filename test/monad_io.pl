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
       | .Foldable (∀a b. (a -> b -> b) -> b -> t a -> b) (∀a b. (b -> a -> b) -> b -> t a -> b)
data Functor f =
       | .Functor (∀a b. (a -> b) -> f a -> f b)
data List a =
       | Nil
       | Cons a (List a)
data Monad m =
       | .Monad (∀a. a -> m a) (∀a b. m a -> (a -> m b) -> m b)
data World =
       | World
data IO a = World -> Pair a World
external lt_int : Int -> Int -> Bool = "lt"
external le_int : Int -> Int -> Bool = "le"
external ge_int : Int -> Int -> Bool = "ge"
external gt_int : Int -> Int -> Bool = "gt"
external neg_int : Int -> Int = "neg"
external add_int : Int -> Int -> Int = "add"
external sub_int : Int -> Int -> Int = "sub"
external mul_int : Int -> Int -> Int = "mul"
external seq : ∀a b. a -> b -> b = "seq"
external puti : Int -> Unit = "puti"
external geti : Unit -> Int = "geti"
ordInt : Ord Int = .Ord @Int ge_int gt_int le_int lt_int
ringInt : Ring Int = .Ring @Int neg_int add_int sub_int mul_int
monadIO : Monad IO = .Monad @IO monadIO.pure.L2 monadIO.bind.L2
input : IO Int = io.L2 @Unit @Int geti Unit
main : IO Unit = bind.L1 @IO monadIO @Int @Unit input main.L2
ge.L1 : ∀a. Ord a -> a -> a -> Bool =
  fun @a (dict : Ord a) ->
    match dict with
    | .Ord ge _ _ _ -> ge
gt.L1 : ∀a. Ord a -> a -> a -> Bool =
  fun @a (dict : Ord a) ->
    match dict with
    | .Ord _ gt _ _ -> gt
sub.L1 : ∀a. Ring a -> a -> a -> a =
  fun @a (dict : Ring a) ->
    match dict with
    | .Ring _ _ sub _ -> sub
pure.L1 : ∀m. Monad m -> (∀a. a -> m a) =
  fun @m (dict : Monad m) ->
    match dict with
    | .Monad pure _ -> pure
bind.L1 : ∀m. Monad m -> (∀a b. m a -> (a -> m b) -> m b) =
  fun @m (dict : Monad m) ->
    match dict with
    | .Monad _ bind -> bind
semi.L1 : ∀a m. m a -> Unit -> m a =
  fun @a @m (m2 : m a) (x : Unit) -> m2
semi.L2 : ∀a m. Monad m -> m Unit -> m a -> m a =
  fun @a @m (monad.m : Monad m) (m1 : m Unit) (m2 : m a) ->
    bind.L1 @m monad.m @Unit @a m1 (semi.L1 @a @m m2)
when.L1 : ∀m. Monad m -> Bool -> m Unit -> m Unit =
  fun @m (monad.m : Monad m) (p : Bool) (m : m Unit) ->
    match p with
    | False -> pure.L1 @m monad.m @Unit Unit
    | True -> m
monadIO.pure.L1 : ∀a. a -> World -> Pair a World =
  fun @a -> Pair @a @World
monadIO.pure.L2 : ∀a. a -> IO a =
  fun @a (x : a) -> coerce @(_ -> IO) (monadIO.pure.L1 @a x)
monadIO.bind.L1 : ∀a b. IO a -> (a -> IO b) -> World -> Pair b World =
  fun @a @b (mx : IO a) (f : a -> IO b) (world0 : World) ->
    match coerce @(IO -> _) mx world0 with
    | Pair x world1 -> coerce @(IO -> _) (f x) world1
monadIO.bind.L2 : ∀a b. IO a -> (a -> IO b) -> IO b =
  fun @a @b (mx : IO a) (f : a -> IO b) ->
    coerce @(_ -> IO) (monadIO.bind.L1 @a @b mx f)
io.L1 : ∀a b. (a -> b) -> a -> World -> Pair b World =
  fun @a @b (f : a -> b) (x : a) (world : World) ->
    let y : b = f x in
    seq @b @(Pair b World) y (Pair @b @World y world)
io.L2 : ∀a b. (a -> b) -> a -> IO b =
  fun @a @b (f : a -> b) (x : a) ->
    coerce @(_ -> IO) (io.L1 @a @b f x)
print.L1 : Int -> IO Unit = io.L2 @Int @Unit puti
count_down.L1 : Int -> IO Unit =
  fun (k : Int) ->
    when.L1 @IO monadIO (ge.L1 @Int ordInt k 0) (semi.L2 @Unit @IO monadIO (print.L1 k) (count_down.L1 (sub.L1 @Int ringInt k 1)))
repeat.L1 : ∀m. Monad m -> Int -> m Unit -> m Unit =
  fun @m (monad.m : Monad m) (k : Int) (m : m Unit) ->
    when.L1 @m monad.m (gt.L1 @Int ordInt k 0) (semi.L2 @Unit @m monad.m m (repeat.L1 @m monad.m (sub.L1 @Int ringInt k 1) m))
main.L1 : Int -> Int -> IO Unit =
  fun (k : Int) (n : Int) ->
    repeat.L1 @IO monadIO k (count_down.L1 n)
main.L2 : Int -> IO Unit =
  fun (k : Int) -> bind.L1 @IO monadIO @Int @Unit input (main.L1 k)
