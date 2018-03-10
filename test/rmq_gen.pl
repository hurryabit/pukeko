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
external mod : Int -> Int -> Int = "mod"
external seq : ∀a b. a -> b -> b = "seq"
external puti : Int -> Unit = "puti"
ordInt : Ord Int = .Ord @Int ge_int gt_int le_int lt_int
ringInt : Ring Int = .Ring @Int neg_int add_int sub_int mul_int
foldableList : Foldable List =
  .Foldable @List foldableList.foldr.L1 foldableList.foldl.L1
monadIO : Monad IO = .Monad @IO monadIO.pure.L2 monadIO.bind.L2
print : Int -> IO Unit = io.L2 @Int @Unit puti
random : List Int = gen.L1 @Int random.L1 1
main : IO Unit =
  let m1 : IO Unit = print 400000
  and m2 : IO Unit =
        let m1 : IO Unit = print 100000
        and m2 : IO Unit =
              match split_at.L1 @Int 400000 random with
              | Pair xs random ->
                let m1 : IO Unit =
                      (match foldableList with
                       | .Foldable foldr _ ->
                         foldr) @Int @(IO Unit) (traverse_.L1 @Int @IO monadIO print) ((match monadIO with
                                                                                        | .Monad pure _ ->
                                                                                          pure) @Unit Unit) xs
                and m2 : IO Unit =
                      match split_at.L1 @Int 100000 random with
                      | Pair ys random ->
                        let zs : List Int = take.L1 @Int 100000 random in
                        (match monadIO with
                         | .Monad _ bind ->
                           bind) @(List Unit) @Unit (sequence.L3 @Unit @IO monadIO (zip_with.L1 @Int @Int @(IO Unit) (main.L1 400000) ys zs)) main.L2
                in
                (match monadIO with
                 | .Monad _ bind -> bind) @Unit @Unit m1 (semi.L1 @Unit @IO m2)
        in
        (match monadIO with
         | .Monad _ bind -> bind) @Unit @Unit m1 (semi.L1 @Unit @IO m2)
  in
  (match monadIO with
   | .Monad _ bind -> bind) @Unit @Unit m1 (semi.L1 @Unit @IO m2)
foldableList.foldr.L1 : ∀a b. (a -> b -> b) -> b -> List a -> b =
  fun @a @b (f : a -> b -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> y0
    | Cons x xs ->
      f x ((match foldableList with
            | .Foldable foldr _ -> foldr) @a @b f y0 xs)
foldableList.foldl.L1 : ∀a b. (b -> a -> b) -> b -> List a -> b =
  fun @a @b (f : b -> a -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> y0
    | Cons x xs ->
      (match foldableList with
       | .Foldable _ foldl -> foldl) @a @b f (f y0 x) xs
take.L1 : ∀a. Int -> List a -> List a =
  fun @a (n : Int) (xs : List a) ->
    match (match ordInt with
           | .Ord _ _ le _ -> le) n 0 with
    | False ->
      match xs with
      | Nil -> Nil @a
      | Cons x xs ->
        Cons @a x (take.L1 @a ((match ringInt with
                                | .Ring _ _ sub _ -> sub) n 1) xs)
    | True -> Nil @a
zip_with.L1 : ∀a b c. (a -> b -> c) -> List a -> List b -> List c =
  fun @a @b @c (f : a -> b -> c) (xs : List a) (ys : List b) ->
    match xs with
    | Nil -> Nil @c
    | Cons x xs ->
      match ys with
      | Nil -> Nil @c
      | Cons y ys -> Cons @c (f x y) (zip_with.L1 @a @b @c f xs ys)
semi.L1 : ∀a m. m a -> Unit -> m a =
  fun @a @m (m2 : m a) (x : Unit) -> m2
semi.L2 : ∀a m. Monad m -> m Unit -> m a -> m a =
  fun @a @m (monad.m : Monad m) (m1 : m Unit) (m2 : m a) ->
    (match monad.m with
     | .Monad _ bind -> bind) @Unit @a m1 (semi.L1 @a @m m2)
sequence.L1 : ∀a m. Monad m -> a -> List a -> m (List a) =
  fun @a @m (monad.m : Monad m) (x : a) (xs : List a) ->
    (match monad.m with
     | .Monad pure _ -> pure) @(List a) (Cons @a x xs)
sequence.L2 : ∀a m. Monad m -> List (m a) -> a -> m (List a) =
  fun @a @m (monad.m : Monad m) (ms : List (m a)) (x : a) ->
    (match monad.m with
     | .Monad _ bind ->
       bind) @(List a) @(List a) (sequence.L3 @a @m monad.m ms) (sequence.L1 @a @m monad.m x)
sequence.L3 : ∀a m. Monad m -> List (m a) -> m (List a) =
  fun @a @m (monad.m : Monad m) (ms : List (m a)) ->
    match ms with
    | Nil ->
      (match monad.m with
       | .Monad pure _ -> pure) @(List a) (Nil @a)
    | Cons m ms ->
      (match monad.m with
       | .Monad _ bind ->
         bind) @a @(List a) m (sequence.L2 @a @m monad.m ms)
traverse_.L1 : ∀a m. Monad m -> (a -> m Unit) -> a -> m Unit -> m Unit =
  fun @a @m (monad.m : Monad m) (f : a -> m Unit) (x : a) ->
    semi.L2 @Unit @m monad.m (f x)
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
gen.L1 : ∀a. (a -> a) -> a -> List a =
  fun @a (f : a -> a) (x : a) -> Cons @a x (gen.L1 @a f (f x))
split_at.L1 : ∀a. Int -> List a -> Pair (List a) (List a) =
  fun @a (n : Int) (xs : List a) ->
    match (match ordInt with
           | .Ord _ _ le _ -> le) n 0 with
    | False ->
      match xs with
      | Nil -> Pair @(List a) @(List a) (Nil @a) (Nil @a)
      | Cons x xs ->
        match split_at.L1 @a ((match ringInt with
                               | .Ring _ _ sub _ -> sub) n 1) xs with
        | Pair ys zs -> Pair @(List a) @(List a) (Cons @a x ys) zs
    | True -> Pair @(List a) @(List a) (Nil @a) xs
random.L1 : Int -> Int =
  fun (x : Int) ->
    mod ((match ringInt with
          | .Ring _ _ _ mul -> mul) 91 x) 1000000007
main.L1 : Int -> Int -> Int -> IO Unit =
  fun (n : Int) (y : Int) (z : Int) ->
    let y : Int = mod y n in
    let z : Int = mod z n in
    match (match ordInt with
           | .Ord _ _ _ lt -> lt) y z with
    | False ->
      let m1 : IO Unit = print z
      and m2 : IO Unit = print y
      in
      (match monadIO with
       | .Monad _ bind -> bind) @Unit @Unit m1 (semi.L1 @Unit @IO m2)
    | True ->
      let m1 : IO Unit = print y
      and m2 : IO Unit = print z
      in
      (match monadIO with
       | .Monad _ bind -> bind) @Unit @Unit m1 (semi.L1 @Unit @IO m2)
main.L2 : List Unit -> IO Unit =
  fun (x : List Unit) ->
    (match monadIO with
     | .Monad pure _ -> pure) @Unit Unit
