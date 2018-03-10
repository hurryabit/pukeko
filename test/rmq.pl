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
print : Int -> IO Unit = io.L2 @Int @Unit puti
input : IO Int =
  let f : Unit -> Int = geti
  and x : Unit = Unit
  in
  coerce @(_ -> IO) (io.L1 @Unit @Int f x)
nats : List Int =
  let rec nats_from : Int -> List Int = nats.L1 nats_from in
  nats_from 0
infinity : Int = 1000000000
main : IO Unit =
  let dict : Monad IO = monadIO in
  (match dict with
   | .Monad _ bind -> bind) @Int @Unit input main.L6
replicate.L1 : ∀a. Int -> a -> List a =
  fun @a (n : Int) (x : a) ->
    match let dict : Ord Int = ordInt in
          (match dict with
           | .Ord _ _ le _ -> le) n 0 with
    | False ->
      Cons @a x (replicate.L1 @a (let dict : Ring Int = ringInt in
                                  (match dict with
                                   | .Ring _ _ sub _ -> sub) n 1) x)
    | True -> Nil @a
zip_with.L1 : ∀a b c. (a -> b -> c) -> List a -> List b -> List c =
  fun @a @b @c (f : a -> b -> c) (xs : List a) (ys : List b) ->
    match xs with
    | Nil -> Nil @c
    | Cons x xs ->
      match ys with
      | Nil -> Nil @c
      | Cons y ys -> Cons @c (f x y) (zip_with.L1 @a @b @c f xs ys)
sequence.L1 : ∀a m. Monad m -> a -> List a -> m (List a) =
  fun @a @m (monad.m : Monad m) (x : a) (xs : List a) ->
    let dict : Monad m = monad.m in
    (match dict with
     | .Monad pure _ -> pure) @(List a) (Cons @a x xs)
sequence.L2 : ∀a m. Monad m -> List (m a) -> a -> m (List a) =
  fun @a @m (monad.m : Monad m) (ms : List (m a)) (x : a) ->
    let dict : Monad m = monad.m in
    (match dict with
     | .Monad _ bind ->
       bind) @(List a) @(List a) (sequence.L3 @a @m monad.m ms) (sequence.L1 @a @m monad.m x)
sequence.L3 : ∀a m. Monad m -> List (m a) -> m (List a) =
  fun @a @m (monad.m : Monad m) (ms : List (m a)) ->
    match ms with
    | Nil ->
      let dict : Monad m = monad.m in
      (match dict with
       | .Monad pure _ -> pure) @(List a) (Nil @a)
    | Cons m ms ->
      let dict : Monad m = monad.m in
      (match dict with
       | .Monad _ bind ->
         bind) @a @(List a) m (sequence.L2 @a @m monad.m ms)
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
nats.L1 : (Int -> List Int) -> Int -> List Int =
  fun (nats_from : Int -> List Int) (n : Int) ->
    Cons @Int n (nats_from (let dict : Ring Int = ringInt in
                            (match dict with
                             | .Ring _ add _ _ -> add) n 1))
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
query.L1 : ∀a. a -> (a -> a -> a) -> Int -> Int -> (RmqTree a -> a) -> RmqTree a -> a =
  fun @a (one : a) (op : a -> a -> a) (q_lo : Int) (q_hi : Int) (aux : RmqTree a -> a) (t : RmqTree a) ->
    match t with
    | RmqEmpty -> one
    | RmqNode t_lo t_hi value left right ->
      match let x : Bool =
                  let dict : Ord Int = ordInt in
                  (match dict with
                   | .Ord _ _ _ lt -> lt) q_hi t_lo
            and y : Bool =
                  let dict : Ord Int = ordInt in
                  (match dict with
                   | .Ord _ gt _ _ -> gt) q_lo t_hi
            in
            match x with
            | False -> y
            | True -> True with
      | False ->
        match let x : Bool =
                    let dict : Ord Int = ordInt in
                    (match dict with
                     | .Ord _ _ le _ -> le) q_lo t_lo
              and y : Bool =
                    let dict : Ord Int = ordInt in
                    (match dict with
                     | .Ord _ _ le _ -> le) t_hi q_hi
              in
              match x with
              | False -> False
              | True -> y with
        | False -> op (aux left) (aux right)
        | True -> value
      | True -> one
min.L1 : Int -> Int -> Int =
  fun (x : Int) (y : Int) ->
    match let dict : Ord Int = ordInt in
          (match dict with
           | .Ord _ _ le _ -> le) x y with
    | False -> y
    | True -> x
main.L1 : RmqTree Int -> Int -> Int -> IO Unit =
  fun (t : RmqTree Int) (lo : Int) (hi : Int) ->
    let res : Int =
          let one : Int = infinity
          and op : Int -> Int -> Int = min.L1
          and q_lo : Int = lo
          and q_hi : Int = hi
          in
          let rec aux : RmqTree Int -> Int =
                    query.L1 @Int one op q_lo q_hi aux
          in
          aux t
    in
    print res
main.L2 : RmqTree Int -> Int -> IO Unit =
  fun (t : RmqTree Int) (lo : Int) ->
    let dict : Monad IO = monadIO in
    (match dict with
     | .Monad _ bind -> bind) @Int @Unit input (main.L1 t lo)
main.L3 : List Unit -> IO Unit =
  fun (x : List Unit) ->
    let dict : Monad IO = monadIO in
    (match dict with
     | .Monad pure _ -> pure) @Unit Unit
main.L4 : Int -> List Int -> IO Unit =
  fun (m : Int) (xs : List Int) ->
    let t : RmqTree Int =
          let op : Int -> Int -> Int = min.L1
          and xs : List Int = xs
          in
          let rec run : List (RmqTree Int) -> RmqTree Int =
                    build.L1 @Int op run
          in
          run (zip_with.L1 @Int @Int @(RmqTree Int) (single.L1 @Int) nats xs)
    in
    let dict : Monad IO = monadIO in
    (match dict with
     | .Monad _ bind -> bind) @(List Unit) @Unit (let n : Int = m
                                                  and act : IO Unit =
                                                        let dict : Monad IO = monadIO in
                                                        (match dict with
                                                         | .Monad _ bind ->
                                                           bind) @Int @Unit input (main.L2 t)
                                                  in
                                                  sequence.L3 @Unit @IO monadIO (replicate.L1 @(IO Unit) n act)) main.L3
main.L5 : Int -> Int -> IO Unit =
  fun (n : Int) (m : Int) ->
    let dict : Monad IO = monadIO in
    (match dict with
     | .Monad _ bind -> bind) @(List Int) @Unit (let n : Int = n
                                                 and act : IO Int = input
                                                 in
                                                 sequence.L3 @Int @IO monadIO (replicate.L1 @(IO Int) n act)) (main.L4 m)
main.L6 : Int -> IO Unit =
  fun (n : Int) ->
    let dict : Monad IO = monadIO in
    (match dict with
     | .Monad _ bind -> bind) @Int @Unit input (main.L5 n)
