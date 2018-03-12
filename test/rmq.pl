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
data RmqTree a =
       | RmqEmpty
       | RmqNode Int Int a (RmqTree a) (RmqTree a)
external abort : ∀a. a = "abort"
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
nats : List Int =
  let rec nats_from : Int -> List Int = nats.L1 nats_from in
  nats_from 0
infinity : Int = 1000000000
main : IO Unit = bind.L1 @IO monadIO @Int @Unit input main.L6
conj.L1 : Bool -> Bool -> Bool =
  fun (x : Bool) (y : Bool) ->
    match x with
    | False -> False
    | True -> y
disj.L1 : Bool -> Bool -> Bool =
  fun (x : Bool) (y : Bool) ->
    match x with
    | False -> y
    | True -> True
gt.L1 : ∀a. Ord a -> a -> a -> Bool =
  fun @a (dict : Ord a) ->
    match dict with
    | .Ord _ gt _ _ -> gt
le.L1 : ∀a. Ord a -> a -> a -> Bool =
  fun @a (dict : Ord a) ->
    match dict with
    | .Ord _ _ le _ -> le
lt.L1 : ∀a. Ord a -> a -> a -> Bool =
  fun @a (dict : Ord a) ->
    match dict with
    | .Ord _ _ _ lt -> lt
add.L1 : ∀a. Ring a -> a -> a -> a =
  fun @a (dict : Ring a) ->
    match dict with
    | .Ring _ add _ _ -> add
sub.L1 : ∀a. Ring a -> a -> a -> a =
  fun @a (dict : Ring a) ->
    match dict with
    | .Ring _ _ sub _ -> sub
replicate.L1 : ∀a. Int -> a -> List a =
  fun @a (n : Int) (x : a) ->
    match le.L1 @Int ordInt n 0 with
    | False -> Cons @a x (replicate.L1 @a (sub.L1 @Int ringInt n 1) x)
    | True -> Nil @a
zip_with.L1 : ∀a b c. (a -> b -> c) -> List a -> List b -> List c =
  fun @a @b @c (f : a -> b -> c) (xs : List a) (ys : List b) ->
    match xs with
    | Nil -> Nil @c
    | Cons x xs ->
      match ys with
      | Nil -> Nil @c
      | Cons y ys -> Cons @c (f x y) (zip_with.L1 @a @b @c f xs ys)
pure.L1 : ∀m. Monad m -> (∀a. a -> m a) =
  fun @m (dict : Monad m) ->
    match dict with
    | .Monad pure _ -> pure
bind.L1 : ∀m. Monad m -> (∀a b. m a -> (a -> m b) -> m b) =
  fun @m (dict : Monad m) ->
    match dict with
    | .Monad _ bind -> bind
sequence.L1 : ∀a m. Monad m -> a -> List a -> m (List a) =
  fun @a @m (monad.m : Monad m) (x : a) (xs : List a) ->
    pure.L1 @m monad.m @(List a) (Cons @a x xs)
sequence.L2 : ∀a m. Monad m -> List (m a) -> a -> m (List a) =
  fun @a @m (monad.m : Monad m) (ms : List (m a)) (x : a) ->
    bind.L1 @m monad.m @(List a) @(List a) (sequence.L3 @a @m monad.m ms) (sequence.L1 @a @m monad.m x)
sequence.L3 : ∀a m. Monad m -> List (m a) -> m (List a) =
  fun @a @m (monad.m : Monad m) (ms : List (m a)) ->
    match ms with
    | Nil -> pure.L1 @m monad.m @(List a) (Nil @a)
    | Cons m ms ->
      bind.L1 @m monad.m @a @(List a) m (sequence.L2 @a @m monad.m ms)
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
nats.L1 : (Int -> List Int) -> Int -> List Int =
  fun (nats_from : Int -> List Int) (n : Int) ->
    Cons @Int n (nats_from (add.L1 @Int ringInt n 1))
pair.L1 : ∀a. (a -> a -> a) -> List a -> List a =
  fun @a (op : a -> a -> a) (xs1 : List a) ->
    match xs1 with
    | Nil -> Nil @a
    | Cons pm$1 pm$2 ->
      match pm$2 with
      | Nil -> xs1
      | Cons x2 xs3 -> Cons @a (op pm$1 x2) (pair.L1 @a op xs3)
single.L1 : ∀a. Int -> a -> RmqTree a =
  fun @a (i : Int) (x : a) ->
    RmqNode @a i i x (RmqEmpty @a) (RmqEmpty @a)
combine.L1 : ∀a. (a -> a -> a) -> RmqTree a -> RmqTree a -> RmqTree a =
  fun @a (op : a -> a -> a) (t1 : RmqTree a) (t2 : RmqTree a) ->
    match t1 with
    | RmqEmpty -> abort @(RmqTree a)
    | RmqNode s1 _ v1 _ _ ->
      match t2 with
      | RmqEmpty -> abort @(RmqTree a)
      | RmqNode _ e2 v2 _ _ -> RmqNode @a s1 e2 (op v1 v2) t1 t2
build.L1 : ∀a. (a -> a -> a) -> (List (RmqTree a) -> RmqTree a) -> List (RmqTree a) -> RmqTree a =
  fun @a (op : a -> a -> a) (run : List (RmqTree a) -> RmqTree a) (ts : List (RmqTree a)) ->
    match ts with
    | Nil -> abort @(RmqTree a)
    | Cons pm$1 pm$2 ->
      match pm$2 with
      | Nil -> pm$1
      | Cons _ _ -> run (pair.L1 @(RmqTree a) (combine.L1 @a op) ts)
build.L2 : ∀a. (a -> a -> a) -> List a -> RmqTree a =
  fun @a (op : a -> a -> a) (xs : List a) ->
    let rec run : List (RmqTree a) -> RmqTree a = build.L1 @a op run in
    run (zip_with.L1 @Int @a @(RmqTree a) (single.L1 @a) nats xs)
query.L1 : ∀a. a -> (a -> a -> a) -> Int -> Int -> (RmqTree a -> a) -> RmqTree a -> a =
  fun @a (one : a) (op : a -> a -> a) (q_lo : Int) (q_hi : Int) (aux : RmqTree a -> a) (t : RmqTree a) ->
    match t with
    | RmqEmpty -> one
    | RmqNode t_lo t_hi value left right ->
      match disj.L1 (lt.L1 @Int ordInt q_hi t_lo) (gt.L1 @Int ordInt q_lo t_hi) with
      | False ->
        match conj.L1 (le.L1 @Int ordInt q_lo t_lo) (le.L1 @Int ordInt t_hi q_hi) with
        | False -> op (aux left) (aux right)
        | True -> value
      | True -> one
query.L2 : ∀a. a -> (a -> a -> a) -> Int -> Int -> RmqTree a -> a =
  fun @a (one : a) (op : a -> a -> a) (q_lo : Int) (q_hi : Int) ->
    let rec aux : RmqTree a -> a = query.L1 @a one op q_lo q_hi aux in
    aux
min.L1 : Int -> Int -> Int =
  fun (x : Int) (y : Int) ->
    match le.L1 @Int ordInt x y with
    | False -> y
    | True -> x
replicate_io.L1 : ∀a. Int -> IO a -> IO (List a) =
  fun @a (n : Int) (act : IO a) ->
    sequence.L3 @a @IO monadIO (replicate.L1 @(IO a) n act)
main.L1 : RmqTree Int -> Int -> Int -> IO Unit =
  fun (t : RmqTree Int) (lo : Int) (hi : Int) ->
    let res : Int = query.L2 @Int infinity min.L1 lo hi t in
    print.L1 res
main.L2 : RmqTree Int -> Int -> IO Unit =
  fun (t : RmqTree Int) (lo : Int) ->
    bind.L1 @IO monadIO @Int @Unit input (main.L1 t lo)
main.L3 : List Unit -> IO Unit =
  fun (x : List Unit) -> pure.L1 @IO monadIO @Unit Unit
main.L4 : Int -> List Int -> IO Unit =
  fun (m : Int) (xs : List Int) ->
    let t : RmqTree Int = build.L2 @Int min.L1 xs in
    bind.L1 @IO monadIO @(List Unit) @Unit (replicate_io.L1 @Unit m (bind.L1 @IO monadIO @Int @Unit input (main.L2 t))) main.L3
main.L5 : Int -> Int -> IO Unit =
  fun (n : Int) (m : Int) ->
    bind.L1 @IO monadIO @(List Int) @Unit (replicate_io.L1 @Int n input) (main.L4 m)
main.L6 : Int -> IO Unit =
  fun (n : Int) -> bind.L1 @IO monadIO @Int @Unit input (main.L5 n)
