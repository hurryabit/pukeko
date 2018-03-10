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
dict$Ord$Int : Ord Int = Dict$Ord @Int ge_int gt_int le_int lt_int
dict$Ring$Int : Ring Int =
  Dict$Ring @Int neg_int add_int sub_int mul_int
dict$Monad$IO : Monad IO =
  Dict$Monad @IO dict$Monad$IO$ll2 dict$Monad$IO$ll4
input : IO Int = coerce @(_ -> IO) (io$ll1 @Unit @Int geti Unit)
main : IO Unit =
  (match dict$Monad$IO with
   | Dict$Monad _ bind -> bind) @Int @Unit input main$ll2
semi$ll1 : ∀a m. m a -> Unit -> m a =
  fun @a @m (m2 : m a) (x : Unit) -> m2
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
count_down$ll1 : Int -> IO Unit =
  fun (k : Int) ->
    let p : Bool =
          (match dict$Ord$Int with
           | Dict$Ord ge _ _ _ -> ge) k 0
    and m : IO Unit =
          let m1 : IO Unit = print$ll1 k
          and m2 : IO Unit =
                count_down$ll1 ((match dict$Ring$Int with
                                 | Dict$Ring _ _ sub _ -> sub) k 1)
          in
          (match dict$Monad$IO with
           | Dict$Monad _ bind -> bind) @Unit @Unit m1 (semi$ll1 @Unit @IO m2)
    in
    match p with
    | False ->
      (match dict$Monad$IO with
       | Dict$Monad pure _ -> pure) @Unit Unit
    | True -> m
repeat$ll1 : ∀m. Monad m -> Int -> m Unit -> m Unit =
  fun @m (dict$Monad$m : Monad m) (k : Int) (m : m Unit) ->
    let p : Bool =
          (match dict$Ord$Int with
           | Dict$Ord _ gt _ _ -> gt) k 0
    and m : m Unit =
          let m2 : m Unit =
                repeat$ll1 @m dict$Monad$m ((match dict$Ring$Int with
                                             | Dict$Ring _ _ sub _ -> sub) k 1) m
          in
          (match dict$Monad$m with
           | Dict$Monad _ bind -> bind) @Unit @Unit m (semi$ll1 @Unit @m m2)
    in
    match p with
    | False ->
      (match dict$Monad$m with
       | Dict$Monad pure _ -> pure) @Unit Unit
    | True -> m
main$ll1 : Int -> Int -> IO Unit =
  fun (k : Int) (n : Int) ->
    repeat$ll1 @IO dict$Monad$IO k (count_down$ll1 n)
main$ll2 : Int -> IO Unit =
  fun (k : Int) ->
    (match dict$Monad$IO with
     | Dict$Monad _ bind -> bind) @Int @Unit input (main$ll1 k)
