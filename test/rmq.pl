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
input : IO Int =
  let f : Unit -> Int = geti
  and x : Unit = Unit
  in
  coerce @(_ -> IO) (io$ll1 @Unit @Int f x)
nats : List Int =
  let rec nats_from : Int -> List Int = nats$ll1 nats_from in
  nats_from 0
infinity : Int = 1000000000
main : IO Unit =
  let dict : Monad IO = dict$Monad$IO in
  (match dict with
   | Dict$Monad _ bind -> bind) @Int @Unit input main$ll6
replicate$ll1 : ∀a. Int -> a -> List a =
  fun @a (n : Int) (x : a) ->
    match let dict : Ord Int = dict$Ord$Int in
          (match dict with
           | Dict$Ord _ _ le _ -> le) n 0 with
    | False ->
      Cons @a x (replicate$ll1 @a (let dict : Ring Int = dict$Ring$Int in
                                   (match dict with
                                    | Dict$Ring _ _ sub _ -> sub) n 1) x)
    | True -> Nil @a
zip_with$ll1 : ∀a b c. (a -> b -> c) -> List a -> List b -> List c =
  fun @a @b @c (f : a -> b -> c) (xs : List a) (ys : List b) ->
    match xs with
    | Nil -> Nil @c
    | Cons x xs ->
      match ys with
      | Nil -> Nil @c
      | Cons y ys -> Cons @c (f x y) (zip_with$ll1 @a @b @c f xs ys)
sequence$ll1 : ∀a m. Monad m -> a -> List a -> m (List a) =
  fun @a @m (dict$Monad$m : Monad m) (x : a) (xs : List a) ->
    let dict : Monad m = dict$Monad$m in
    (match dict with
     | Dict$Monad pure _ -> pure) @(List a) (Cons @a x xs)
sequence$ll2 : ∀a m. Monad m -> List (m a) -> a -> m (List a) =
  fun @a @m (dict$Monad$m : Monad m) (ms : List (m a)) (x : a) ->
    let dict : Monad m = dict$Monad$m in
    (match dict with
     | Dict$Monad _ bind ->
       bind) @(List a) @(List a) (sequence$ll3 @a @m dict$Monad$m ms) (sequence$ll1 @a @m dict$Monad$m x)
sequence$ll3 : ∀a m. Monad m -> List (m a) -> m (List a) =
  fun @a @m (dict$Monad$m : Monad m) (ms : List (m a)) ->
    match ms with
    | Nil ->
      let dict : Monad m = dict$Monad$m in
      (match dict with
       | Dict$Monad pure _ -> pure) @(List a) (Nil @a)
    | Cons m ms ->
      let dict : Monad m = dict$Monad$m in
      (match dict with
       | Dict$Monad _ bind ->
         bind) @a @(List a) m (sequence$ll2 @a @m dict$Monad$m ms)
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
nats$ll1 : (Int -> List Int) -> Int -> List Int =
  fun (nats_from : Int -> List Int) (n : Int) ->
    Cons @Int n (nats_from (let dict : Ring Int = dict$Ring$Int in
                            (match dict with
                             | Dict$Ring _ add _ _ -> add) n 1))
pair$ll1 : ∀a. (a -> a -> a) -> List a -> List a =
  fun @a (op : a -> a -> a) (xs1 : List a) ->
    match xs1 with
    | Nil -> Nil @a
    | Cons pm$1 pm$2 ->
      match pm$2 with
      | Nil -> xs1
      | Cons x2 xs3 -> Cons @a (op pm$1 x2) (pair$ll1 @a op xs3)
single$ll1 : ∀a. Int -> a -> RmqTree a =
  fun @a (i : Int) (x : a) ->
    RmqNode @a i i x (RmqEmpty @a) (RmqEmpty @a)
combine$ll1 : ∀a. (a -> a -> a) -> RmqTree a -> RmqTree a -> RmqTree a =
  fun @a (op : a -> a -> a) (t1 : RmqTree a) (t2 : RmqTree a) ->
    match t1 with
    | RmqEmpty -> abort @(RmqTree a)
    | RmqNode s1 _ v1 _ _ ->
      match t2 with
      | RmqEmpty -> abort @(RmqTree a)
      | RmqNode _ e2 v2 _ _ -> RmqNode @a s1 e2 (op v1 v2) t1 t2
build$ll1 : ∀a. (a -> a -> a) -> (List (RmqTree a) -> RmqTree a) -> List (RmqTree a) -> RmqTree a =
  fun @a (op : a -> a -> a) (run : List (RmqTree a) -> RmqTree a) (ts : List (RmqTree a)) ->
    match ts with
    | Nil -> abort @(RmqTree a)
    | Cons pm$1 pm$2 ->
      match pm$2 with
      | Nil -> pm$1
      | Cons _ _ -> run (pair$ll1 @(RmqTree a) (combine$ll1 @a op) ts)
query$ll1 : ∀a. a -> (a -> a -> a) -> Int -> Int -> (RmqTree a -> a) -> RmqTree a -> a =
  fun @a (one : a) (op : a -> a -> a) (q_lo : Int) (q_hi : Int) (aux : RmqTree a -> a) (t : RmqTree a) ->
    match t with
    | RmqEmpty -> one
    | RmqNode t_lo t_hi value left right ->
      match let x : Bool =
                  let dict : Ord Int = dict$Ord$Int in
                  (match dict with
                   | Dict$Ord _ _ _ lt -> lt) q_hi t_lo
            and y : Bool =
                  let dict : Ord Int = dict$Ord$Int in
                  (match dict with
                   | Dict$Ord _ gt _ _ -> gt) q_lo t_hi
            in
            match x with
            | False -> y
            | True -> True with
      | False ->
        match let x : Bool =
                    let dict : Ord Int = dict$Ord$Int in
                    (match dict with
                     | Dict$Ord _ _ le _ -> le) q_lo t_lo
              and y : Bool =
                    let dict : Ord Int = dict$Ord$Int in
                    (match dict with
                     | Dict$Ord _ _ le _ -> le) t_hi q_hi
              in
              match x with
              | False -> False
              | True -> y with
        | False -> op (aux left) (aux right)
        | True -> value
      | True -> one
min$ll1 : Int -> Int -> Int =
  fun (x : Int) (y : Int) ->
    match let dict : Ord Int = dict$Ord$Int in
          (match dict with
           | Dict$Ord _ _ le _ -> le) x y with
    | False -> y
    | True -> x
main$ll1 : RmqTree Int -> Int -> Int -> IO Unit =
  fun (t : RmqTree Int) (lo : Int) (hi : Int) ->
    let res : Int =
          let one : Int = infinity
          and op : Int -> Int -> Int = min$ll1
          and q_lo : Int = lo
          and q_hi : Int = hi
          in
          let rec aux : RmqTree Int -> Int =
                    query$ll1 @Int one op q_lo q_hi aux
          in
          aux t
    in
    print$ll1 res
main$ll2 : RmqTree Int -> Int -> IO Unit =
  fun (t : RmqTree Int) (lo : Int) ->
    let dict : Monad IO = dict$Monad$IO in
    (match dict with
     | Dict$Monad _ bind -> bind) @Int @Unit input (main$ll1 t lo)
main$ll3 : List Unit -> IO Unit =
  fun (x : List Unit) ->
    let dict : Monad IO = dict$Monad$IO in
    (match dict with
     | Dict$Monad pure _ -> pure) @Unit Unit
main$ll4 : Int -> List Int -> IO Unit =
  fun (m : Int) (xs : List Int) ->
    let t : RmqTree Int =
          let op : Int -> Int -> Int = min$ll1
          and xs : List Int = xs
          in
          let rec run : List (RmqTree Int) -> RmqTree Int =
                    build$ll1 @Int op run
          in
          run (zip_with$ll1 @Int @Int @(RmqTree Int) (single$ll1 @Int) nats xs)
    in
    let dict : Monad IO = dict$Monad$IO in
    (match dict with
     | Dict$Monad _ bind -> bind) @(List Unit) @Unit (let n : Int = m
                                                      and act : IO Unit =
                                                            let dict : Monad IO = dict$Monad$IO in
                                                            (match dict with
                                                             | Dict$Monad _ bind ->
                                                               bind) @Int @Unit input (main$ll2 t)
                                                      in
                                                      sequence$ll3 @Unit @IO dict$Monad$IO (replicate$ll1 @(IO Unit) n act)) main$ll3
main$ll5 : Int -> Int -> IO Unit =
  fun (n : Int) (m : Int) ->
    let dict : Monad IO = dict$Monad$IO in
    (match dict with
     | Dict$Monad _ bind -> bind) @(List Int) @Unit (let n : Int = n
                                                     and act : IO Int = input
                                                     in
                                                     sequence$ll3 @Int @IO dict$Monad$IO (replicate$ll1 @(IO Int) n act)) (main$ll4 m)
main$ll6 : Int -> IO Unit =
  fun (n : Int) ->
    let dict : Monad IO = dict$Monad$IO in
    (match dict with
     | Dict$Monad _ bind -> bind) @Int @Unit input (main$ll5 n)
