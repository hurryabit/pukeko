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
external mod : Int -> Int -> Int = "mod"
external seq : ∀a b. a -> b -> b = "seq"
external puti : Int -> Unit = "puti"
dict$Ord$Int : Ord Int = Dict$Ord @Int ge_int gt_int le_int lt_int
dict$Ring$Int : Ring Int =
  Dict$Ring @Int neg_int add_int sub_int mul_int
dict$Foldable$List : Foldable List =
  Dict$Foldable @List dict$Foldable$List$ll1 dict$Foldable$List$ll2
dict$Monad$IO : Monad IO =
  Dict$Monad @IO dict$Monad$IO$ll2 dict$Monad$IO$ll4
random : List Int = gen$ll1 @Int random$ll1 1
main : IO Unit =
  let m1 : IO Unit = print$ll1 400000
  and m2 : IO Unit =
        let m1 : IO Unit = print$ll1 100000
        and m2 : IO Unit =
              match split_at$ll1 @Int 400000 random with
              | Pair xs random ->
                let m1 : IO Unit =
                      (match dict$Foldable$List with
                       | Dict$Foldable foldr _ ->
                         foldr) @Int @(IO Unit) (traverse_$ll1 @Int @IO dict$Monad$IO print$ll1) ((match dict$Monad$IO with
                                                                                                   | Dict$Monad pure _ ->
                                                                                                     pure) @Unit Unit) xs
                and m2 : IO Unit =
                      match split_at$ll1 @Int 100000 random with
                      | Pair ys random ->
                        let zs : List Int = take$ll1 @Int 100000 random in
                        (match dict$Monad$IO with
                         | Dict$Monad _ bind ->
                           bind) @(List Unit) @Unit (sequence$ll3 @Unit @IO dict$Monad$IO (zip_with$ll1 @Int @Int @(IO Unit) (main$ll1 400000) ys zs)) main$ll2
                in
                (match dict$Monad$IO with
                 | Dict$Monad _ bind -> bind) @Unit @Unit m1 (semi$ll1 @Unit @IO m2)
        in
        (match dict$Monad$IO with
         | Dict$Monad _ bind -> bind) @Unit @Unit m1 (semi$ll1 @Unit @IO m2)
  in
  (match dict$Monad$IO with
   | Dict$Monad _ bind -> bind) @Unit @Unit m1 (semi$ll1 @Unit @IO m2)
dict$Foldable$List$ll1 : ∀a b. (a -> b -> b) -> b -> List a -> b =
  fun @a @b (f : a -> b -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> y0
    | Cons x xs ->
      f x ((match dict$Foldable$List with
            | Dict$Foldable foldr _ -> foldr) @a @b f y0 xs)
dict$Foldable$List$ll2 : ∀a b. (b -> a -> b) -> b -> List a -> b =
  fun @a @b (f : b -> a -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> y0
    | Cons x xs ->
      (match dict$Foldable$List with
       | Dict$Foldable _ foldl -> foldl) @a @b f (f y0 x) xs
take$ll1 : ∀a. Int -> List a -> List a =
  fun @a (n : Int) (xs : List a) ->
    match (match dict$Ord$Int with
           | Dict$Ord _ _ le _ -> le) n 0 with
    | False ->
      match xs with
      | Nil -> Nil @a
      | Cons x xs ->
        Cons @a x (take$ll1 @a ((match dict$Ring$Int with
                                 | Dict$Ring _ _ sub _ -> sub) n 1) xs)
    | True -> Nil @a
zip_with$ll1 : ∀a b c. (a -> b -> c) -> List a -> List b -> List c =
  fun @a @b @c (f : a -> b -> c) (xs : List a) (ys : List b) ->
    match xs with
    | Nil -> Nil @c
    | Cons x xs ->
      match ys with
      | Nil -> Nil @c
      | Cons y ys -> Cons @c (f x y) (zip_with$ll1 @a @b @c f xs ys)
semi$ll1 : ∀a m. m a -> Unit -> m a =
  fun @a @m (m2 : m a) (x : Unit) -> m2
semi$ll2 : ∀a m. Monad m -> m Unit -> m a -> m a =
  fun @a @m (dict$Monad$m : Monad m) (m1 : m Unit) (m2 : m a) ->
    (match dict$Monad$m with
     | Dict$Monad _ bind -> bind) @Unit @a m1 (semi$ll1 @a @m m2)
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
traverse_$ll1 : ∀a m. Monad m -> (a -> m Unit) -> a -> m Unit -> m Unit =
  fun @a @m (dict$Monad$m : Monad m) (f : a -> m Unit) (x : a) ->
    semi$ll2 @Unit @m dict$Monad$m (f x)
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
gen$ll1 : ∀a. (a -> a) -> a -> List a =
  fun @a (f : a -> a) (x : a) -> Cons @a x (gen$ll1 @a f (f x))
split_at$ll1 : ∀a. Int -> List a -> Pair (List a) (List a) =
  fun @a (n : Int) (xs : List a) ->
    match (match dict$Ord$Int with
           | Dict$Ord _ _ le _ -> le) n 0 with
    | False ->
      match xs with
      | Nil -> Pair @(List a) @(List a) (Nil @a) (Nil @a)
      | Cons x xs ->
        match split_at$ll1 @a ((match dict$Ring$Int with
                                | Dict$Ring _ _ sub _ -> sub) n 1) xs with
        | Pair ys zs -> Pair @(List a) @(List a) (Cons @a x ys) zs
    | True -> Pair @(List a) @(List a) (Nil @a) xs
random$ll1 : Int -> Int =
  fun (x : Int) ->
    mod ((match dict$Ring$Int with
          | Dict$Ring _ _ _ mul -> mul) 91 x) 1000000007
main$ll1 : Int -> Int -> Int -> IO Unit =
  fun (n : Int) (y : Int) (z : Int) ->
    let y : Int = mod y n in
    let z : Int = mod z n in
    match (match dict$Ord$Int with
           | Dict$Ord _ _ _ lt -> lt) y z with
    | False ->
      let m1 : IO Unit = print$ll1 z
      and m2 : IO Unit = print$ll1 y
      in
      (match dict$Monad$IO with
       | Dict$Monad _ bind -> bind) @Unit @Unit m1 (semi$ll1 @Unit @IO m2)
    | True ->
      let m1 : IO Unit = print$ll1 y
      and m2 : IO Unit = print$ll1 z
      in
      (match dict$Monad$IO with
       | Dict$Monad _ bind -> bind) @Unit @Unit m1 (semi$ll1 @Unit @IO m2)
main$ll2 : List Unit -> IO Unit =
  fun (x : List Unit) ->
    (match dict$Monad$IO with
     | Dict$Monad pure _ -> pure) @Unit Unit
