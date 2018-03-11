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
external gt_int : Int -> Int -> Bool = "gt"
external add_int : Int -> Int -> Int = "add"
external sub_int : Int -> Int -> Int = "sub"
external seq : ∀a b. a -> b -> b = "seq"
external puti : Int -> Unit = "puti"
external geti : Unit -> Int = "geti"
dict$Monad$IO : Monad IO =
  Dict$Monad @IO dict$Monad$IO$ll2 dict$Monad$IO$ll4
input : IO Int = coerce @(_ -> IO) (io$ll1 @Unit @Int geti Unit)
nats : List Int =
  let rec nats_from : Int -> List Int = nats$ll1 nats_from in
  nats_from 0
infinity : Int = 1000000000
main : IO Unit =
  coerce @(_ -> IO) (dict$Monad$IO$ll3 @Int @Unit input main$ll6)
replicate$ll1 : ∀a. Int -> a -> List a =
  fun @a (n : Int) (x : a) ->
    match le_int n 0 with
    | False -> Cons @a x (replicate$ll1 @a (sub_int n 1) x)
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
    (match dict$Monad$m with
     | Dict$Monad pure _ -> pure) @(List a) (Cons @a x xs)
sequence$ll2 : ∀a m. Monad m -> List (m a) -> a -> m (List a) =
  fun @a @m (dict$Monad$m : Monad m) (ms : List (m a)) (x : a) ->
    (match dict$Monad$m with
     | Dict$Monad _ bind ->
       bind) @(List a) @(List a) (sequence$ll3 @a @m dict$Monad$m ms) (sequence$ll1 @a @m dict$Monad$m x)
sequence$ll3 : ∀a m. Monad m -> List (m a) -> m (List a) =
  fun @a @m (dict$Monad$m : Monad m) (ms : List (m a)) ->
    match ms with
    | Nil ->
      (match dict$Monad$m with
       | Dict$Monad pure _ -> pure) @(List a) (Nil @a)
    | Cons m ms ->
      (match dict$Monad$m with
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
    Cons @Int n (nats_from (add_int n 1))
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
      match let x : Bool = lt_int q_hi t_lo
            and y : Bool = gt_int q_lo t_hi
            in
            match x with
            | False -> y
            | True -> True with
      | False ->
        match let x : Bool = le_int q_lo t_lo
              and y : Bool = le_int t_hi q_hi
              in
              match x with
              | False -> False
              | True -> y with
        | False -> op (aux left) (aux right)
        | True -> value
      | True -> one
min$ll1 : Int -> Int -> Int =
  fun (x : Int) (y : Int) ->
    match le_int x y with
    | False -> y
    | True -> x
main$ll1 : RmqTree Int -> Int -> Int -> IO Unit =
  fun (t : RmqTree Int) (lo : Int) (hi : Int) ->
    let res : Int =
          let rec aux : RmqTree Int -> Int =
                    query$ll1 @Int infinity min$ll1 lo hi aux
          in
          aux t
    in
    print$ll1 res
main$ll2 : RmqTree Int -> Int -> IO Unit =
  fun (t : RmqTree Int) (lo : Int) ->
    let f : Int -> IO Unit = main$ll1 t lo in
    coerce @(_ -> IO) (dict$Monad$IO$ll3 @Int @Unit input f)
main$ll3 : List Unit -> IO Unit =
  fun (x : List Unit) -> coerce @(_ -> IO) (Pair @Unit @World Unit)
main$ll4 : Int -> List Int -> IO Unit =
  fun (m : Int) (xs : List Int) ->
    let t : RmqTree Int =
          let rec run : List (RmqTree Int) -> RmqTree Int =
                    build$ll1 @Int min$ll1 run
          in
          run (zip_with$ll1 @Int @Int @(RmqTree Int) (single$ll1 @Int) nats xs)
    in
    let mx : IO (List Unit) =
          let act : IO Unit =
                let f : Int -> IO Unit = main$ll2 t in
                coerce @(_ -> IO) (dict$Monad$IO$ll3 @Int @Unit input f)
          in
          sequence$ll3 @Unit @IO dict$Monad$IO (replicate$ll1 @(IO Unit) m act)
    in
    coerce @(_ -> IO) (dict$Monad$IO$ll3 @(List Unit) @Unit mx main$ll3)
main$ll5 : Int -> Int -> IO Unit =
  fun (n : Int) (m : Int) ->
    let mx : IO (List Int) =
          sequence$ll3 @Int @IO dict$Monad$IO (replicate$ll1 @(IO Int) n input)
    and f : List Int -> IO Unit = main$ll4 m
    in
    coerce @(_ -> IO) (dict$Monad$IO$ll3 @(List Int) @Unit mx f)
main$ll6 : Int -> IO Unit =
  fun (n : Int) ->
    let f : Int -> IO Unit = main$ll5 n in
    coerce @(_ -> IO) (dict$Monad$IO$ll3 @Int @Unit input f)
