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
external add_int : Int -> Int -> Int = "add"
external ge_int : Int -> Int -> Bool = "ge"
external gt_int : Int -> Int -> Bool = "gt"
external le_int : Int -> Int -> Bool = "le"
external lt_int : Int -> Int -> Bool = "lt"
external mod : Int -> Int -> Int = "mod"
external mul_int : Int -> Int -> Int = "mul"
external neg_int : Int -> Int = "neg"
external puti : Int -> Unit = "puti"
external seq : ∀a b. a -> b -> b = "seq"
external sub_int : Int -> Int -> Int = "sub"
bind$ll1 : ∀m. Monad m -> (∀a b. m a -> (a -> m b) -> m b) =
  fun @m ->
    fun (dict : Monad m) ->
      match dict with
      | Dict$Monad @m _ bind -> bind
dict$Foldable$List : Foldable List =
  let foldr : ∀a b. (a -> b -> b) -> b -> List a -> b =
        fun @a @b -> dict$Foldable$List$ll1 @a @b
  and foldl : ∀a b. (b -> a -> b) -> b -> List a -> b =
        fun @a @b -> dict$Foldable$List$ll2 @a @b
  in
  Dict$Foldable @List foldr foldl
dict$Foldable$List$ll1 : ∀a b. (a -> b -> b) -> b -> List a -> b =
  fun @a @b ->
    fun (f : a -> b -> b) (y0 : b) (xs : List a) ->
      match xs with
      | Nil @a -> y0
      | Cons @a x xs ->
        f x (foldr$ll1 @List dict$Foldable$List @a @b f y0 xs)
dict$Foldable$List$ll2 : ∀a b. (b -> a -> b) -> b -> List a -> b =
  fun @a @b ->
    fun (f : b -> a -> b) (y0 : b) (xs : List a) ->
      match xs with
      | Nil @a -> y0
      | Cons @a x xs ->
        foldl$ll1 @List dict$Foldable$List @a @b f (f y0 x) xs
dict$Monad$IO : Monad IO =
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
foldl$ll1 : ∀t. Foldable t -> (∀a b. (b -> a -> b) -> b -> t a -> b) =
  fun @t ->
    fun (dict : Foldable t) ->
      match dict with
      | Dict$Foldable @t _ foldl -> foldl
foldr$ll1 : ∀t. Foldable t -> (∀a b. (a -> b -> b) -> b -> t a -> b) =
  fun @t ->
    fun (dict : Foldable t) ->
      match dict with
      | Dict$Foldable @t foldr _ -> foldr
gen$ll1 : ∀a. (a -> a) -> a -> List a =
  fun @a ->
    fun (f : a -> a) (x : a) -> Cons @a x (gen$ll1 @a f (f x))
io$ll1 : ∀a b. (a -> b) -> a -> World -> Pair b World =
  fun @a @b ->
    fun (f : a -> b) (x : a) (world : World) ->
      let y : b = f x in
      seq @b @(Pair b World) y (Pair @b @World y world)
io$ll2 : ∀a b. (a -> b) -> a -> IO b =
  fun @a @b ->
    fun (f : a -> b) (x : a) -> coerce @(_ -> IO) (io$ll1 @a @b f x)
le$ll1 : ∀a. Ord a -> a -> a -> Bool =
  fun @a ->
    fun (dict : Ord a) ->
      match dict with
      | Dict$Ord @a _ _ le _ -> le
lt$ll1 : ∀a. Ord a -> a -> a -> Bool =
  fun @a ->
    fun (dict : Ord a) ->
      match dict with
      | Dict$Ord @a _ _ _ lt -> lt
main : IO Unit =
  let n : Int = 400000 in
  semi$ll2 @Unit @IO dict$Monad$IO (print$ll1 n) (let m : Int =
                                                        100000
                                                  in
                                                  semi$ll2 @Unit @IO dict$Monad$IO (print$ll1 m) (match split_at$ll1 @Int n random with
                                                                                                  | Pair @(List Int) @(List Int) xs random ->
                                                                                                    semi$ll2 @Unit @IO dict$Monad$IO (traverse_$ll2 @Int @IO @List dict$Monad$IO dict$Foldable$List print$ll1 xs) (match split_at$ll1 @Int m random with
                                                                                                                                                                                                                   | Pair @(List Int) @(List Int) ys random ->
                                                                                                                                                                                                                     let zs : List Int =
                                                                                                                                                                                                                           take$ll1 @Int m random
                                                                                                                                                                                                                     in
                                                                                                                                                                                                                     bind$ll1 @IO dict$Monad$IO @(List Unit) @Unit (sequence$ll3 @Unit @IO dict$Monad$IO (zip_with$ll1 @Int @Int @(IO Unit) (main$ll1 n) ys zs)) main$ll2)))
main$ll1 : Int -> Int -> Int -> IO Unit =
  fun (n : Int) (y : Int) (z : Int) ->
    let y : Int = mod y n in
    let z : Int = mod z n in
    match lt$ll1 @Int dict$Ord$Int y z with
    | False ->
      semi$ll2 @Unit @IO dict$Monad$IO (print$ll1 z) (print$ll1 y)
    | True ->
      semi$ll2 @Unit @IO dict$Monad$IO (print$ll1 y) (print$ll1 z)
main$ll2 : List Unit -> IO Unit =
  fun (x : List Unit) -> pure$ll1 @IO dict$Monad$IO @Unit Unit
mul$ll1 : ∀a. Ring a -> a -> a -> a =
  fun @a ->
    fun (dict : Ring a) ->
      match dict with
      | Dict$Ring @a _ _ _ mul -> mul
print$ll1 : Int -> IO Unit =
  fun (n : Int) -> io$ll2 @Int @Unit puti n
pure$ll1 : ∀m. Monad m -> (∀a. a -> m a) =
  fun @m ->
    fun (dict : Monad m) ->
      match dict with
      | Dict$Monad @m pure _ -> pure
random : List Int = gen$ll1 @Int random$ll1 1
random$ll1 : Int -> Int =
  fun (x : Int) -> mod (mul$ll1 @Int dict$Ring$Int 91 x) 1000000007
semi$ll1 : ∀a m. m a -> Unit -> m a =
  fun @a @m -> fun (m2 : m a) (x : Unit) -> m2
semi$ll2 : ∀a m. Monad m -> m Unit -> m a -> m a =
  fun @a @m ->
    fun (dict$Monad$m : Monad m) (m1 : m Unit) (m2 : m a) ->
      bind$ll1 @m dict$Monad$m @Unit @a m1 (semi$ll1 @a @m m2)
sequence$ll1 : ∀a m. Monad m -> a -> List a -> m (List a) =
  fun @a @m ->
    fun (dict$Monad$m : Monad m) (x : a) (xs : List a) ->
      pure$ll1 @m dict$Monad$m @(List a) (Cons @a x xs)
sequence$ll2 : ∀a m. Monad m -> List (m a) -> a -> m (List a) =
  fun @a @m ->
    fun (dict$Monad$m : Monad m) (ms : List (m a)) (x : a) ->
      bind$ll1 @m dict$Monad$m @(List a) @(List a) (sequence$ll3 @a @m dict$Monad$m ms) (sequence$ll1 @a @m dict$Monad$m x)
sequence$ll3 : ∀a m. Monad m -> List (m a) -> m (List a) =
  fun @a @m ->
    fun (dict$Monad$m : Monad m) (ms : List (m a)) ->
      match ms with
      | Nil @(m a) -> pure$ll1 @m dict$Monad$m @(List a) (Nil @a)
      | Cons @(m a) m ms ->
        bind$ll1 @m dict$Monad$m @a @(List a) m (sequence$ll2 @a @m dict$Monad$m ms)
split_at$ll1 : ∀a. Int -> List a -> Pair (List a) (List a) =
  fun @a ->
    fun (n : Int) (xs : List a) ->
      match le$ll1 @Int dict$Ord$Int n 0 with
      | False ->
        match xs with
        | Nil @a -> Pair @(List a) @(List a) (Nil @a) (Nil @a)
        | Cons @a x xs ->
          match split_at$ll1 @a (sub$ll1 @Int dict$Ring$Int n 1) xs with
          | Pair @(List a) @(List a) ys zs ->
            Pair @(List a) @(List a) (Cons @a x ys) zs
      | True -> Pair @(List a) @(List a) (Nil @a) xs
sub$ll1 : ∀a. Ring a -> a -> a -> a =
  fun @a ->
    fun (dict : Ring a) ->
      match dict with
      | Dict$Ring @a _ _ sub _ -> sub
take$ll1 : ∀a. Int -> List a -> List a =
  fun @a ->
    fun (n : Int) (xs : List a) ->
      match le$ll1 @Int dict$Ord$Int n 0 with
      | False ->
        match xs with
        | Nil @a -> Nil @a
        | Cons @a x xs ->
          Cons @a x (take$ll1 @a (sub$ll1 @Int dict$Ring$Int n 1) xs)
      | True -> Nil @a
traverse_$ll1 : ∀a m. Monad m -> (a -> m Unit) -> a -> m Unit -> m Unit =
  fun @a @m ->
    fun (dict$Monad$m : Monad m) (f : a -> m Unit) (x : a) (m : m Unit) ->
      semi$ll2 @Unit @m dict$Monad$m (f x) m
traverse_$ll2 : ∀a m t. Monad m -> Foldable t -> (a -> m Unit) -> t a -> m Unit =
  fun @a @m @t ->
    fun (dict$Monad$m : Monad m) (dict$Foldable$t : Foldable t) (f : a -> m Unit) ->
      foldr$ll1 @t dict$Foldable$t @a @(m Unit) (traverse_$ll1 @a @m dict$Monad$m f) (pure$ll1 @m dict$Monad$m @Unit Unit)
zip_with$ll1 : ∀a b c. (a -> b -> c) -> List a -> List b -> List c =
  fun @a @b @c ->
    fun (f : a -> b -> c) (xs : List a) (ys : List b) ->
      match xs with
      | Nil @a -> Nil @c
      | Cons @a x xs ->
        match ys with
        | Nil @b -> Nil @c
        | Cons @b y ys -> Cons @c (f x y) (zip_with$ll1 @a @b @c f xs ys)
