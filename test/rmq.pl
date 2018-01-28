external abort : ∀a. a = "abort"
type Unit =
       | Unit
type Bool =
       | False
       | True
type Pair a b =
       | Pair a b
type Option a =
       | None
       | Some a
type Choice a b =
       | First a
       | Second b
type Dict$Eq a =
       | Dict$Eq (a -> a -> Bool)
(&&) : Bool -> Bool -> Bool =
  fun (x : Bool) (y : Bool) ->
    match x with
    | False -> False
    | True -> y
(||) : Bool -> Bool -> Bool =
  fun (x : Bool) (y : Bool) ->
    match x with
    | False -> y
    | True -> True
type Dict$Ord a =
       | Dict$Ord (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool)
(<) : ∀a. Dict$Ord a -> a -> a -> Bool =
  fun @a ->
    fun (dict : Dict$Ord a) ->
      match dict with
      | Dict$Ord @a (<) (<=) (>=) (>) -> (<)
(<=) : ∀a. Dict$Ord a -> a -> a -> Bool =
  fun @a ->
    fun (dict : Dict$Ord a) ->
      match dict with
      | Dict$Ord @a (<) (<=) (>=) (>) -> (<=)
(>) : ∀a. Dict$Ord a -> a -> a -> Bool =
  fun @a ->
    fun (dict : Dict$Ord a) ->
      match dict with
      | Dict$Ord @a (<) (<=) (>=) (>) -> (>)
type Dict$Monoid m =
       | Dict$Monoid m (m -> m -> m)
type Dict$Ring a =
       | Dict$Ring (a -> a) (a -> a -> a) (a -> a -> a) (a -> a -> a)
(+) : ∀a. Dict$Ring a -> a -> a -> a =
  fun @a ->
    fun (dict : Dict$Ring a) ->
      match dict with
      | Dict$Ring @a neg (+) (-) (*) -> (+)
(-) : ∀a. Dict$Ring a -> a -> a -> a =
  fun @a ->
    fun (dict : Dict$Ring a) ->
      match dict with
      | Dict$Ring @a neg (+) (-) (*) -> (-)
type Int
external lt_int : Int -> Int -> Bool = "lt"
external le_int : Int -> Int -> Bool = "le"
external ge_int : Int -> Int -> Bool = "ge"
external gt_int : Int -> Int -> Bool = "gt"
dict$Ord$Int : Dict$Ord Int =
  let (<) : Int -> Int -> Bool = lt_int
  and (<=) : Int -> Int -> Bool = le_int
  and (>=) : Int -> Int -> Bool = ge_int
  and (>) : Int -> Int -> Bool = gt_int
  in
  Dict$Ord @Int (<) (<=) (>=) (>)
external neg_int : Int -> Int = "neg"
external add_int : Int -> Int -> Int = "add"
external sub_int : Int -> Int -> Int = "sub"
external mul_int : Int -> Int -> Int = "mul"
dict$Ring$Int : Dict$Ring Int =
  let neg : Int -> Int = neg_int
  and (+) : Int -> Int -> Int = add_int
  and (-) : Int -> Int -> Int = sub_int
  and (*) : Int -> Int -> Int = mul_int
  in
  Dict$Ring @Int neg (+) (-) (*)
type Dict$Foldable t =
       | Dict$Foldable (∀a b. (a -> b -> b) -> b -> t a -> b) (∀a b. (b -> a -> b) -> b -> t a -> b)
type Dict$Functor f =
       | Dict$Functor (∀a b. (a -> b) -> f a -> f b)
type List a =
       | Nil
       | Cons a (List a)
replicate : ∀a. Int -> a -> List a =
  fun @a ->
    fun (n : Int) (x : a) ->
      match (<=) @Int dict$Ord$Int n 0 with
      | False -> Cons @a x (replicate @a ((-) @Int dict$Ring$Int n 1) x)
      | True -> Nil @a
zip_with : ∀a b c. (a -> b -> c) -> List a -> List b -> List c =
  fun @a @b @c ->
    fun (f : a -> b -> c) (xs : List a) (ys : List b) ->
      match xs with
      | Nil @a -> Nil @c
      | Cons @a x xs ->
        match ys with
        | Nil @b -> Nil @c
        | Cons @b y ys -> Cons @c (f x y) (zip_with @a @b @c f xs ys)
type Dict$Monad m =
       | Dict$Monad (∀a. a -> m a) (∀a b. m a -> (a -> m b) -> m b)
pure : ∀m. Dict$Monad m -> (∀a. a -> m a) =
  fun @m ->
    fun (dict : Dict$Monad m) ->
      match dict with
      | Dict$Monad @m pure (>>=) -> pure
(>>=) : ∀m. Dict$Monad m -> (∀a b. m a -> (a -> m b) -> m b) =
  fun @m ->
    fun (dict : Dict$Monad m) ->
      match dict with
      | Dict$Monad @m pure (>>=) -> (>>=)
sequence$ll1 : ∀a m. Dict$Monad m -> a -> List a -> m (List a) =
  fun @a @m ->
    fun (dict$Monad$m : Dict$Monad m) (x : a) (xs : List a) ->
      pure @m dict$Monad$m @(List a) (Cons @a x xs)
sequence$ll2 : ∀a m. Dict$Monad m -> List (m a) -> a -> m (List a) =
  fun @a @m ->
    fun (dict$Monad$m : Dict$Monad m) (ms : List (m a)) (x : a) ->
      (>>=) @m dict$Monad$m @(List a) @(List a) (sequence @a @m dict$Monad$m ms) (sequence$ll1 @a @m dict$Monad$m x)
sequence : ∀a m. Dict$Monad m -> List (m a) -> m (List a) =
  fun @a @m ->
    fun (dict$Monad$m : Dict$Monad m) (ms : List (m a)) ->
      match ms with
      | Nil @(m a) -> pure @m dict$Monad$m @(List a) (Nil @a)
      | Cons @(m a) m ms ->
        (>>=) @m dict$Monad$m @a @(List a) m (sequence$ll2 @a @m dict$Monad$m ms)
type IO a
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
nats$ll1 : (Int -> List Int) -> Int -> List Int =
  fun (nats_from : Int -> List Int) (n : Int) ->
    Cons @Int n (nats_from ((+) @Int dict$Ring$Int n 1))
nats : List Int =
  let rec nats_from : Int -> List Int = nats$ll1 nats_from in
  nats_from 0
pair : ∀a. (a -> a -> a) -> List a -> List a =
  fun @a ->
    fun (op : a -> a -> a) (xs1 : List a) ->
      match xs1 with
      | Nil @a -> Nil @a
      | Cons @a pair$pm1 pair$pm2 ->
        match pair$pm2 with
        | Nil @a -> xs1
        | Cons @a x2 xs3 -> Cons @a (op pair$pm1 x2) (pair @a op xs3)
type RmqTree a =
       | RmqEmpty
       | RmqNode Int Int a (RmqTree a) (RmqTree a)
single : ∀a. Int -> a -> RmqTree a =
  fun @a ->
    fun (i : Int) (x : a) ->
      RmqNode @a i i x (RmqEmpty @a) (RmqEmpty @a)
combine : ∀a. (a -> a -> a) -> RmqTree a -> RmqTree a -> RmqTree a =
  fun @a ->
    fun (op : a -> a -> a) (t1 : RmqTree a) (t2 : RmqTree a) ->
      match t1 with
      | RmqEmpty @a -> abort @(RmqTree a)
      | RmqNode @a s1 combine$pm1 v1 combine$pm2 combine$pm3 ->
        match t2 with
        | RmqEmpty @a -> abort @(RmqTree a)
        | RmqNode @a combine$pm4 e2 v2 combine$pm5 combine$pm6 ->
          RmqNode @a s1 e2 (op v1 v2) t1 t2
build$ll1 : ∀a. (a -> a -> a) -> (List (RmqTree a) -> RmqTree a) -> List (RmqTree a) -> RmqTree a =
  fun @a ->
    fun (op : a -> a -> a) (run : List (RmqTree a) -> RmqTree a) (ts : List (RmqTree a)) ->
      match ts with
      | Nil @(RmqTree a) -> abort @(RmqTree a)
      | Cons @(RmqTree a) build$pm1 build$pm2 ->
        match build$pm2 with
        | Nil @(RmqTree a) -> build$pm1
        | Cons @(RmqTree a) build$pm3 build$pm4 ->
          run (pair @(RmqTree a) (combine @a op) ts)
build : ∀a. (a -> a -> a) -> List a -> RmqTree a =
  fun @a ->
    fun (op : a -> a -> a) (xs : List a) ->
      let rec run : List (RmqTree a) -> RmqTree a = build$ll1 @a op run
      in
      run (zip_with @Int @a @(RmqTree a) (single @a) nats xs)
query$ll1 : ∀a. a -> (a -> a -> a) -> Int -> Int -> (RmqTree a -> a) -> RmqTree a -> a =
  fun @a ->
    fun (one : a) (op : a -> a -> a) (q_lo : Int) (q_hi : Int) (aux : RmqTree a -> a) (t : RmqTree a) ->
      match t with
      | RmqEmpty @a -> one
      | RmqNode @a t_lo t_hi value left right ->
        match (||) ((<) @Int dict$Ord$Int q_hi t_lo) ((>) @Int dict$Ord$Int q_lo t_hi) with
        | False ->
          match (&&) ((<=) @Int dict$Ord$Int q_lo t_lo) ((<=) @Int dict$Ord$Int t_hi q_hi) with
          | False -> op (aux left) (aux right)
          | True -> value
        | True -> one
query : ∀a. a -> (a -> a -> a) -> Int -> Int -> RmqTree a -> a =
  fun @a ->
    fun (one : a) (op : a -> a -> a) (q_lo : Int) (q_hi : Int) ->
      let rec aux : RmqTree a -> a = query$ll1 @a one op q_lo q_hi aux in
      aux
infinity : Int = 1000000000
min : Int -> Int -> Int =
  fun (x : Int) (y : Int) ->
    match (<=) @Int dict$Ord$Int x y with
    | False -> y
    | True -> x
replicate_io : ∀a. Int -> IO a -> IO (List a) =
  fun @a ->
    fun (n : Int) (act : IO a) ->
      sequence @a @IO dict$Monad$IO (replicate @(IO a) n act)
main$ll1 : RmqTree Int -> Int -> Int -> IO Unit =
  fun (t : RmqTree Int) (lo : Int) (hi : Int) ->
    let res : Int = query @Int infinity min lo hi t in
    print res
main$ll2 : RmqTree Int -> Int -> IO Unit =
  fun (t : RmqTree Int) (lo : Int) ->
    (>>=) @IO dict$Monad$IO @Int @Unit input (main$ll1 t lo)
main$ll3 : List Unit -> IO Unit =
  fun (x : List Unit) -> pure @IO dict$Monad$IO @Unit Unit
main$ll4 : Int -> List Int -> IO Unit =
  fun (m : Int) (xs : List Int) ->
    let t : RmqTree Int = build @Int min xs in
    (>>=) @IO dict$Monad$IO @(List Unit) @Unit (replicate_io @Unit m ((>>=) @IO dict$Monad$IO @Int @Unit input (main$ll2 t))) main$ll3
main$ll5 : Int -> Int -> IO Unit =
  fun (n : Int) (m : Int) ->
    (>>=) @IO dict$Monad$IO @(List Int) @Unit (replicate_io @Int n input) (main$ll4 m)
main$ll6 : Int -> IO Unit =
  fun (n : Int) ->
    (>>=) @IO dict$Monad$IO @Int @Unit input (main$ll5 n)
main : IO Unit = (>>=) @IO dict$Monad$IO @Int @Unit input main$ll6
