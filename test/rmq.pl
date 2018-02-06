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
data RmqTree a =
       | RmqEmpty
       | RmqNode Int Int a (RmqTree a) (RmqTree a)
data Unit =
       | Unit
data World =
       | World
external abort : ∀a. a = "abort"
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
add : ∀a. Dict$Ring a -> a -> a -> a =
  fun @a ->
    fun (dict : Dict$Ring a) ->
      match dict with
      | Dict$Ring @a _ add _ _ -> add
bind : ∀m. Dict$Monad m -> (∀a b. m a -> (a -> m b) -> m b) =
  fun @m ->
    fun (dict : Dict$Monad m) ->
      match dict with
      | Dict$Monad @m _ bind -> bind
build : ∀a. (a -> a -> a) -> List a -> RmqTree a =
  fun @a ->
    fun (op : a -> a -> a) (xs : List a) ->
      let rec run : List (RmqTree a) -> RmqTree a = build$ll1 @a op run
      in
      run (zip_with @Int @a @(RmqTree a) (single @a) nats xs)
build$ll1 : ∀a. (a -> a -> a) -> (List (RmqTree a) -> RmqTree a) -> List (RmqTree a) -> RmqTree a =
  fun @a ->
    fun (op : a -> a -> a) (run : List (RmqTree a) -> RmqTree a) (ts : List (RmqTree a)) ->
      match ts with
      | Nil @(RmqTree a) -> abort @(RmqTree a)
      | Cons @(RmqTree a) build$pm1 build$pm2 ->
        match build$pm2 with
        | Nil @(RmqTree a) -> build$pm1
        | Cons @(RmqTree a) _ _ ->
          run (pair @(RmqTree a) (combine @a op) ts)
combine : ∀a. (a -> a -> a) -> RmqTree a -> RmqTree a -> RmqTree a =
  fun @a ->
    fun (op : a -> a -> a) (t1 : RmqTree a) (t2 : RmqTree a) ->
      match t1 with
      | RmqEmpty @a -> abort @(RmqTree a)
      | RmqNode @a s1 _ v1 _ _ ->
        match t2 with
        | RmqEmpty @a -> abort @(RmqTree a)
        | RmqNode @a _ e2 v2 _ _ -> RmqNode @a s1 e2 (op v1 v2) t1 t2
conj : Bool -> Bool -> Bool =
  fun (x : Bool) (y : Bool) ->
    match x with
    | False -> False
    | True -> y
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
disj : Bool -> Bool -> Bool =
  fun (x : Bool) (y : Bool) ->
    match x with
    | False -> y
    | True -> True
gt : ∀a. Dict$Ord a -> a -> a -> Bool =
  fun @a ->
    fun (dict : Dict$Ord a) ->
      match dict with
      | Dict$Ord @a _ gt _ _ -> gt
infinity : Int = 1000000000
input : IO Int = io @Unit @Int geti Unit
io : ∀a b. (a -> b) -> a -> IO b =
  fun @a @b ->
    fun (f : a -> b) (x : a) -> coerce @(_ -> IO) (io$ll1 @a @b f x)
io$ll1 : ∀a b. (a -> b) -> a -> World -> Pair b World =
  fun @a @b ->
    fun (f : a -> b) (x : a) (world : World) ->
      let y : b = f x in
      seq @b @(Pair b World) y (Pair @b @World y world)
le : ∀a. Dict$Ord a -> a -> a -> Bool =
  fun @a ->
    fun (dict : Dict$Ord a) ->
      match dict with
      | Dict$Ord @a _ _ le _ -> le
lt : ∀a. Dict$Ord a -> a -> a -> Bool =
  fun @a ->
    fun (dict : Dict$Ord a) ->
      match dict with
      | Dict$Ord @a _ _ _ lt -> lt
main : IO Unit = bind @IO dict$Monad$IO @Int @Unit input main$ll6
main$ll1 : RmqTree Int -> Int -> Int -> IO Unit =
  fun (t : RmqTree Int) (lo : Int) (hi : Int) ->
    let res : Int = query @Int infinity min lo hi t in
    print res
main$ll2 : RmqTree Int -> Int -> IO Unit =
  fun (t : RmqTree Int) (lo : Int) ->
    bind @IO dict$Monad$IO @Int @Unit input (main$ll1 t lo)
main$ll3 : List Unit -> IO Unit =
  fun (x : List Unit) -> pure @IO dict$Monad$IO @Unit Unit
main$ll4 : Int -> List Int -> IO Unit =
  fun (m : Int) (xs : List Int) ->
    let t : RmqTree Int = build @Int min xs in
    bind @IO dict$Monad$IO @(List Unit) @Unit (replicate_io @Unit m (bind @IO dict$Monad$IO @Int @Unit input (main$ll2 t))) main$ll3
main$ll5 : Int -> Int -> IO Unit =
  fun (n : Int) (m : Int) ->
    bind @IO dict$Monad$IO @(List Int) @Unit (replicate_io @Int n input) (main$ll4 m)
main$ll6 : Int -> IO Unit =
  fun (n : Int) ->
    bind @IO dict$Monad$IO @Int @Unit input (main$ll5 n)
min : Int -> Int -> Int =
  fun (x : Int) (y : Int) ->
    match le @Int dict$Ord$Int x y with
    | False -> y
    | True -> x
nats : List Int =
  let rec nats_from : Int -> List Int = nats$ll1 nats_from in
  nats_from 0
nats$ll1 : (Int -> List Int) -> Int -> List Int =
  fun (nats_from : Int -> List Int) (n : Int) ->
    Cons @Int n (nats_from (add @Int dict$Ring$Int n 1))
pair : ∀a. (a -> a -> a) -> List a -> List a =
  fun @a ->
    fun (op : a -> a -> a) (xs1 : List a) ->
      match xs1 with
      | Nil @a -> Nil @a
      | Cons @a pair$pm1 pair$pm2 ->
        match pair$pm2 with
        | Nil @a -> xs1
        | Cons @a x2 xs3 -> Cons @a (op pair$pm1 x2) (pair @a op xs3)
print : Int -> IO Unit = fun (n : Int) -> io @Int @Unit puti n
pure : ∀m. Dict$Monad m -> (∀a. a -> m a) =
  fun @m ->
    fun (dict : Dict$Monad m) ->
      match dict with
      | Dict$Monad @m pure _ -> pure
query : ∀a. a -> (a -> a -> a) -> Int -> Int -> RmqTree a -> a =
  fun @a ->
    fun (one : a) (op : a -> a -> a) (q_lo : Int) (q_hi : Int) ->
      let rec aux : RmqTree a -> a = query$ll1 @a one op q_lo q_hi aux in
      aux
query$ll1 : ∀a. a -> (a -> a -> a) -> Int -> Int -> (RmqTree a -> a) -> RmqTree a -> a =
  fun @a ->
    fun (one : a) (op : a -> a -> a) (q_lo : Int) (q_hi : Int) (aux : RmqTree a -> a) (t : RmqTree a) ->
      match t with
      | RmqEmpty @a -> one
      | RmqNode @a t_lo t_hi value left right ->
        match disj (lt @Int dict$Ord$Int q_hi t_lo) (gt @Int dict$Ord$Int q_lo t_hi) with
        | False ->
          match conj (le @Int dict$Ord$Int q_lo t_lo) (le @Int dict$Ord$Int t_hi q_hi) with
          | False -> op (aux left) (aux right)
          | True -> value
        | True -> one
replicate : ∀a. Int -> a -> List a =
  fun @a ->
    fun (n : Int) (x : a) ->
      match le @Int dict$Ord$Int n 0 with
      | False -> Cons @a x (replicate @a (sub @Int dict$Ring$Int n 1) x)
      | True -> Nil @a
replicate_io : ∀a. Int -> IO a -> IO (List a) =
  fun @a ->
    fun (n : Int) (act : IO a) ->
      sequence @a @IO dict$Monad$IO (replicate @(IO a) n act)
sequence : ∀a m. Dict$Monad m -> List (m a) -> m (List a) =
  fun @a @m ->
    fun (dict$Monad$m : Dict$Monad m) (ms : List (m a)) ->
      match ms with
      | Nil @(m a) -> pure @m dict$Monad$m @(List a) (Nil @a)
      | Cons @(m a) m ms ->
        bind @m dict$Monad$m @a @(List a) m (sequence$ll2 @a @m dict$Monad$m ms)
sequence$ll1 : ∀a m. Dict$Monad m -> a -> List a -> m (List a) =
  fun @a @m ->
    fun (dict$Monad$m : Dict$Monad m) (x : a) (xs : List a) ->
      pure @m dict$Monad$m @(List a) (Cons @a x xs)
sequence$ll2 : ∀a m. Dict$Monad m -> List (m a) -> a -> m (List a) =
  fun @a @m ->
    fun (dict$Monad$m : Dict$Monad m) (ms : List (m a)) (x : a) ->
      bind @m dict$Monad$m @(List a) @(List a) (sequence @a @m dict$Monad$m ms) (sequence$ll1 @a @m dict$Monad$m x)
single : ∀a. Int -> a -> RmqTree a =
  fun @a ->
    fun (i : Int) (x : a) ->
      RmqNode @a i i x (RmqEmpty @a) (RmqEmpty @a)
sub : ∀a. Dict$Ring a -> a -> a -> a =
  fun @a ->
    fun (dict : Dict$Ring a) ->
      match dict with
      | Dict$Ring @a _ _ sub _ -> sub
zip_with : ∀a b c. (a -> b -> c) -> List a -> List b -> List c =
  fun @a @b @c ->
    fun (f : a -> b -> c) (xs : List a) (ys : List b) ->
      match xs with
      | Nil @a -> Nil @c
      | Cons @a x xs ->
        match ys with
        | Nil @b -> Nil @c
        | Cons @b y ys -> Cons @c (f x y) (zip_with @a @b @c f xs ys)
