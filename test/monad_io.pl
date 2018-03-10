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
print : Int -> IO Unit = io.L2 @Int @Unit puti
input : IO Int =
  let f : Unit -> Int = geti
  and x : Unit = Unit
  in
  coerce @(_ -> IO) (io.L1 @Unit @Int f x)
count_down : Int -> IO Unit =
  fun (k : Int) ->
    let monad.m : Monad IO = monadIO
    and p : Bool =
          let dict : Ord Int = ordInt in
          (match dict with
           | .Ord ge _ _ _ -> ge) k 0
    and m : IO Unit =
          let monad.m : Monad IO = monadIO
          and m1 : IO Unit = print k
          and m2 : IO Unit =
                count_down (let dict : Ring Int = ringInt in
                            (match dict with
                             | .Ring _ _ sub _ -> sub) k 1)
          in
          let dict : Monad IO = monad.m in
          (match dict with
           | .Monad _ bind -> bind) @Unit @Unit m1 (semi.L1 @Unit @IO m2)
    in
    match p with
    | False ->
      let dict : Monad IO = monad.m in
      (match dict with
       | .Monad pure _ -> pure) @Unit Unit
    | True -> m
main : IO Unit =
  let dict : Monad IO = monadIO in
  (match dict with
   | .Monad _ bind -> bind) @Int @Unit input main.L2
semi.L1 : ∀a m. m a -> Unit -> m a =
  fun @a @m (m2 : m a) (x : Unit) -> m2
monadIO.pure.L2 : ∀a. a -> IO a =
  fun @a (x : a) -> coerce @(_ -> IO) (Pair @a @World x)
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
repeat.L1 : ∀m. Monad m -> Int -> m Unit -> m Unit =
  fun @m (monad.m : Monad m) (k : Int) (m : m Unit) ->
    let monad.m : Monad m = monad.m
    and p : Bool =
          let dict : Ord Int = ordInt in
          (match dict with
           | .Ord _ gt _ _ -> gt) k 0
    and m : m Unit =
          let monad.m : Monad m = monad.m
          and m1 : m Unit = m
          and m2 : m Unit =
                repeat.L1 @m monad.m (let dict : Ring Int = ringInt in
                                      (match dict with
                                       | .Ring _ _ sub _ -> sub) k 1) m
          in
          let dict : Monad m = monad.m in
          (match dict with
           | .Monad _ bind -> bind) @Unit @Unit m1 (semi.L1 @Unit @m m2)
    in
    match p with
    | False ->
      let dict : Monad m = monad.m in
      (match dict with
       | .Monad pure _ -> pure) @Unit Unit
    | True -> m
main.L1 : Int -> Int -> IO Unit =
  fun (k : Int) (n : Int) -> repeat.L1 @IO monadIO k (count_down n)
main.L2 : Int -> IO Unit =
  fun (k : Int) ->
    let dict : Monad IO = monadIO in
    (match dict with
     | .Monad _ bind -> bind) @Int @Unit input (main.L1 k)
