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
dict$Ord$Int : Ord Int =
  let ge : Int -> Int -> Bool = ge_int
  and gt : Int -> Int -> Bool = gt_int
  and le : Int -> Int -> Bool = le_int
  and lt : Int -> Int -> Bool = lt_int
  in
  Dict$Ord @Int ge gt le lt
dict$Ring$Int : Ring Int =
  let neg : Int -> Int = neg_int
  and add : Int -> Int -> Int = add_int
  and sub : Int -> Int -> Int = sub_int
  and mul : Int -> Int -> Int = mul_int
  in
  Dict$Ring @Int neg add sub mul
dict$Monad$IO : Monad IO =
  let pure : ∀a. a -> IO a = dict$Monad$IO$ll2
  and bind : ∀a b. IO a -> (a -> IO b) -> IO b = dict$Monad$IO$ll4
  in
  Dict$Monad @IO pure bind
input : IO Int = io$ll2 @Unit @Int geti Unit
main : IO Unit =
  bind$ll1 @IO dict$Monad$IO @Int @Unit input main$ll2
ge$ll1 : ∀a. Ord a -> a -> a -> Bool =
  fun @a (dict : Ord a) ->
    match dict with
    | Dict$Ord @a ge _ _ _ -> ge
gt$ll1 : ∀a. Ord a -> a -> a -> Bool =
  fun @a (dict : Ord a) ->
    match dict with
    | Dict$Ord @a _ gt _ _ -> gt
sub$ll1 : ∀a. Ring a -> a -> a -> a =
  fun @a (dict : Ring a) ->
    match dict with
    | Dict$Ring @a _ _ sub _ -> sub
pure$ll1 : ∀m. Monad m -> (∀a. a -> m a) =
  fun @m (dict : Monad m) ->
    match dict with
    | Dict$Monad @m pure _ -> pure
bind$ll1 : ∀m. Monad m -> (∀a b. m a -> (a -> m b) -> m b) =
  fun @m (dict : Monad m) ->
    match dict with
    | Dict$Monad @m _ bind -> bind
semi$ll1 : ∀a m. m a -> Unit -> m a =
  fun @a @m (m2 : m a) (x : Unit) -> m2
semi$ll2 : ∀a m. Monad m -> m Unit -> m a -> m a =
  fun @a @m (dict$Monad$m : Monad m) (m1 : m Unit) (m2 : m a) ->
    bind$ll1 @m dict$Monad$m @Unit @a m1 (semi$ll1 @a @m m2)
when$ll1 : ∀m. Monad m -> Bool -> m Unit -> m Unit =
  fun @m (dict$Monad$m : Monad m) (p : Bool) (m : m Unit) ->
    match p with
    | False -> pure$ll1 @m dict$Monad$m @Unit Unit
    | True -> m
dict$Monad$IO$ll1 : ∀a. a -> World -> Pair a World =
  fun @a -> Pair @a @World
dict$Monad$IO$ll2 : ∀a. a -> IO a =
  fun @a (x : a) -> coerce @(_ -> IO) (dict$Monad$IO$ll1 @a x)
dict$Monad$IO$ll3 : ∀a b. IO a -> (a -> IO b) -> World -> Pair b World =
  fun @a @b (mx : IO a) (f : a -> IO b) (world0 : World) ->
    match coerce @(IO -> _) mx world0 with
    | Pair @a @World x world1 -> coerce @(IO -> _) (f x) world1
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
    when$ll1 @IO dict$Monad$IO (ge$ll1 @Int dict$Ord$Int k 0) (semi$ll2 @Unit @IO dict$Monad$IO (print$ll1 k) (count_down$ll1 (sub$ll1 @Int dict$Ring$Int k 1)))
repeat$ll1 : ∀m. Monad m -> Int -> m Unit -> m Unit =
  fun @m (dict$Monad$m : Monad m) (k : Int) (m : m Unit) ->
    when$ll1 @m dict$Monad$m (gt$ll1 @Int dict$Ord$Int k 0) (semi$ll2 @Unit @m dict$Monad$m m (repeat$ll1 @m dict$Monad$m (sub$ll1 @Int dict$Ring$Int k 1) m))
main$ll1 : Int -> Int -> IO Unit =
  fun (k : Int) (n : Int) ->
    repeat$ll1 @IO dict$Monad$IO k (count_down$ll1 n)
main$ll2 : Int -> IO Unit =
  fun (k : Int) ->
    bind$ll1 @IO dict$Monad$IO @Int @Unit input (main$ll1 k)
