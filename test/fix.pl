data Bool =
       | False
       | True
data Char
data Choice a b =
       | First a
       | Second b
data Dict$Bifunctor p =
       | Dict$Bifunctor (∀a1 a2 b1 b2. (a1 -> a2) -> (b1 -> b2) -> p a1 b1 -> p a2 b2)
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
data Fix f = f (Fix f)
data Fix2 p a = p a (Fix2 p a)
data FixPoly p a = Fix (p a)
data IO a = World -> Pair a World
data Int
data List a =
       | Nil
       | Cons a (List a)
data ListF a b =
       | NilF
       | ConsF a b
data Option a =
       | None
       | Some a
data Pair a b =
       | Pair a b
data Unit =
       | Unit
data World =
       | World
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
ana$ll1 : ∀a f. Dict$Functor f -> (a -> f a) -> a -> Fix f =
  fun @a @f ->
    fun (dict$Functor$f : Dict$Functor f) (f : a -> f a) ->
      compose$ll1 @a @(f (Fix f)) @(Fix f) (fix$ll1 @f) (compose$ll1 @a @(f a) @(f (Fix f)) (map$ll1 @f dict$Functor$f @a @(Fix f) (ana$ll1 @a @f dict$Functor$f f)) f)
bimap$ll1 : ∀p. Dict$Bifunctor p -> (∀a1 a2 b1 b2. (a1 -> a2) -> (b1 -> b2) -> p a1 b1 -> p a2 b2) =
  fun @p ->
    fun (dict : Dict$Bifunctor p) ->
      match dict with
      | Dict$Bifunctor @p bimap -> bimap
bind$ll1 : ∀m. Dict$Monad m -> (∀a b. m a -> (a -> m b) -> m b) =
  fun @m ->
    fun (dict : Dict$Monad m) ->
      match dict with
      | Dict$Monad @m _ bind -> bind
cata$ll1 : ∀a f. Dict$Functor f -> (f a -> a) -> Fix f -> a =
  fun @a @f ->
    fun (dict$Functor$f : Dict$Functor f) (f : f a -> a) ->
      compose$ll1 @(Fix f) @(f a) @a f (compose$ll1 @(Fix f) @(f (Fix f)) @(f a) (map$ll1 @f dict$Functor$f @(Fix f) @a (cata$ll1 @a @f dict$Functor$f f)) (unFix$ll1 @f))
compose$ll1 : ∀a b c. (b -> c) -> (a -> b) -> a -> c =
  fun @a @b @c -> fun (f : b -> c) (g : a -> b) (x : a) -> f (g x)
dict$Bifunctor$ListF : Dict$Bifunctor ListF =
  let bimap : ∀a b c d. (a -> b) -> (c -> d) -> ListF a c -> ListF b d =
        fun @a @b @c @d -> dict$Bifunctor$ListF$ll1 @a @b @c @d
  in
  Dict$Bifunctor @ListF bimap
dict$Bifunctor$ListF$ll1 : ∀a b c d. (a -> b) -> (c -> d) -> ListF a c -> ListF b d =
  fun @a @b @c @d ->
    fun (f : a -> b) (g : c -> d) (x : ListF a c) ->
      match x with
      | NilF @a @c -> NilF @b @d
      | ConsF @a @c y z -> ConsF @b @d (f y) (g z)
dict$Foldable$List : Dict$Foldable List =
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
dict$Functor$Fix2$ll1 : ∀p a b. Dict$Bifunctor p -> (a -> b) -> Fix2 p a -> Fix2 p b =
  fun @p @a @b ->
    fun (dict$Bifunctor$p : Dict$Bifunctor p) (f : a -> b) ->
      compose$ll1 @(Fix2 p a) @(p b (Fix2 p b)) @(Fix2 p b) (fix2$ll1 @b @p) (compose$ll1 @(Fix2 p a) @(p a (Fix2 p a)) @(p b (Fix2 p b)) (bimap$ll1 @p dict$Bifunctor$p @a @b @(Fix2 p a) @(Fix2 p b) f (map$ll1 @(Fix2 p) (dict$Functor$Fix2$ll2 @p dict$Bifunctor$p) @a @b f)) (unFix2$ll1 @a @p))
dict$Functor$Fix2$ll2 : ∀p. Dict$Bifunctor p -> Dict$Functor (Fix2 p) =
  fun @p ->
    fun (dict$Bifunctor$p : Dict$Bifunctor p) ->
      let map : ∀a b. (a -> b) -> Fix2 p a -> Fix2 p b =
            fun @a @b -> dict$Functor$Fix2$ll1 @p @a @b dict$Bifunctor$p
      in
      Dict$Functor @(Fix2 p) map
dict$Functor$ListF : ∀a. Dict$Functor (ListF a) =
  fun @a ->
    let map : ∀b c. (b -> c) -> ListF a b -> ListF a c =
          fun @b @c ->
            bimap$ll1 @ListF dict$Bifunctor$ListF @a @a @b @c (id$ll1 @a)
    in
    Dict$Functor @(ListF a) map
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
fix$ll1 : ∀f. f (Fix f) -> Fix f =
  fun @f -> fun (x : f (Fix f)) -> coerce @(_ -> Fix) x
fix2$ll1 : ∀a p. p a (Fix2 p a) -> Fix2 p a =
  fun @a @p -> fun (x : p a (Fix2 p a)) -> coerce @(_ -> Fix2) x
foldl$ll1 : ∀t. Dict$Foldable t -> (∀a b. (b -> a -> b) -> b -> t a -> b) =
  fun @t ->
    fun (dict : Dict$Foldable t) ->
      match dict with
      | Dict$Foldable @t _ foldl -> foldl
foldr$ll1 : ∀t. Dict$Foldable t -> (∀a b. (a -> b -> b) -> b -> t a -> b) =
  fun @t ->
    fun (dict : Dict$Foldable t) ->
      match dict with
      | Dict$Foldable @t foldr _ -> foldr
fromList : ∀a. List a -> Fix2 ListF a =
  fun @a ->
    compose$ll1 @(List a) @(Fix (ListF a)) @(Fix2 ListF a) (poly$ll1 @a @ListF dict$Bifunctor$ListF) (ana$ll1 @(List a) @(ListF a) (dict$Functor$ListF @a) (fromList$ll1 @a))
fromList$ll1 : ∀a. List a -> ListF a (List a) =
  fun @a ->
    fun (x : List a) ->
      match x with
      | Nil @a -> NilF @a @(List a)
      | Cons @a y ys -> ConsF @a @(List a) y ys
id$ll1 : ∀a. a -> a = fun @a -> fun (x : a) -> x
input : IO Int = io$ll2 @Unit @Int geti Unit
io$ll1 : ∀a b. (a -> b) -> a -> World -> Pair b World =
  fun @a @b ->
    fun (f : a -> b) (x : a) (world : World) ->
      let y : b = f x in
      seq @b @(Pair b World) y (Pair @b @World y world)
io$ll2 : ∀a b. (a -> b) -> a -> IO b =
  fun @a @b ->
    fun (f : a -> b) (x : a) -> coerce @(_ -> IO) (io$ll1 @a @b f x)
le$ll1 : ∀a. Dict$Ord a -> a -> a -> Bool =
  fun @a ->
    fun (dict : Dict$Ord a) ->
      match dict with
      | Dict$Ord @a _ _ le _ -> le
main : IO Unit =
  bind$ll1 @IO dict$Monad$IO @Int @Unit input main$ll3
main$ll1 : Int -> Int =
  fun (x : Int) -> mul$ll1 @Int dict$Ring$Int 2 x
main$ll2 : List Int -> IO Unit =
  fun (xs : List Int) ->
    traverse_$ll2 @Int @IO @List dict$Monad$IO dict$Foldable$List print$ll1 (toList @Int (map$ll1 @(Fix2 ListF) (dict$Functor$Fix2$ll2 @ListF dict$Bifunctor$ListF) @Int @Int main$ll1 (fromList @Int xs)))
main$ll3 : Int -> IO Unit =
  fun (n : Int) ->
    bind$ll1 @IO dict$Monad$IO @(List Int) @Unit (sequence$ll3 @Int @IO dict$Monad$IO (replicate$ll1 @(IO Int) n input)) main$ll2
map$ll1 : ∀f. Dict$Functor f -> (∀a b. (a -> b) -> f a -> f b) =
  fun @f ->
    fun (dict : Dict$Functor f) ->
      match dict with
      | Dict$Functor @f map -> map
mono$ll1 : ∀a p. Dict$Bifunctor p -> Fix2 p a -> Fix (p a) =
  fun @a @p ->
    fun (dict$Bifunctor$p : Dict$Bifunctor p) ->
      compose$ll1 @(Fix2 p a) @(p a (Fix (p a))) @(Fix (p a)) (fix$ll1 @(p a)) (compose$ll1 @(Fix2 p a) @(p a (Fix2 p a)) @(p a (Fix (p a))) (bimap$ll1 @p dict$Bifunctor$p @a @a @(Fix2 p a) @(Fix (p a)) (id$ll1 @a) (mono$ll1 @a @p dict$Bifunctor$p)) (unFix2$ll1 @a @p))
mul$ll1 : ∀a. Dict$Ring a -> a -> a -> a =
  fun @a ->
    fun (dict : Dict$Ring a) ->
      match dict with
      | Dict$Ring @a _ _ _ mul -> mul
poly$ll1 : ∀a p. Dict$Bifunctor p -> Fix (p a) -> Fix2 p a =
  fun @a @p ->
    fun (dict$Bifunctor$p : Dict$Bifunctor p) ->
      compose$ll1 @(Fix (p a)) @(p a (Fix2 p a)) @(Fix2 p a) (fix2$ll1 @a @p) (compose$ll1 @(Fix (p a)) @(p a (Fix (p a))) @(p a (Fix2 p a)) (bimap$ll1 @p dict$Bifunctor$p @a @a @(Fix (p a)) @(Fix2 p a) (id$ll1 @a) (poly$ll1 @a @p dict$Bifunctor$p)) (unFix$ll1 @(p a)))
print$ll1 : Int -> IO Unit =
  fun (n : Int) -> io$ll2 @Int @Unit puti n
pure$ll1 : ∀m. Dict$Monad m -> (∀a. a -> m a) =
  fun @m ->
    fun (dict : Dict$Monad m) ->
      match dict with
      | Dict$Monad @m pure _ -> pure
replicate$ll1 : ∀a. Int -> a -> List a =
  fun @a ->
    fun (n : Int) (x : a) ->
      match le$ll1 @Int dict$Ord$Int n 0 with
      | False ->
        Cons @a x (replicate$ll1 @a (sub$ll1 @Int dict$Ring$Int n 1) x)
      | True -> Nil @a
semi$ll1 : ∀a m. m a -> Unit -> m a =
  fun @a @m -> fun (m2 : m a) (x : Unit) -> m2
semi$ll2 : ∀a m. Dict$Monad m -> m Unit -> m a -> m a =
  fun @a @m ->
    fun (dict$Monad$m : Dict$Monad m) (m1 : m Unit) (m2 : m a) ->
      bind$ll1 @m dict$Monad$m @Unit @a m1 (semi$ll1 @a @m m2)
sequence$ll1 : ∀a m. Dict$Monad m -> a -> List a -> m (List a) =
  fun @a @m ->
    fun (dict$Monad$m : Dict$Monad m) (x : a) (xs : List a) ->
      pure$ll1 @m dict$Monad$m @(List a) (Cons @a x xs)
sequence$ll2 : ∀a m. Dict$Monad m -> List (m a) -> a -> m (List a) =
  fun @a @m ->
    fun (dict$Monad$m : Dict$Monad m) (ms : List (m a)) (x : a) ->
      bind$ll1 @m dict$Monad$m @(List a) @(List a) (sequence$ll3 @a @m dict$Monad$m ms) (sequence$ll1 @a @m dict$Monad$m x)
sequence$ll3 : ∀a m. Dict$Monad m -> List (m a) -> m (List a) =
  fun @a @m ->
    fun (dict$Monad$m : Dict$Monad m) (ms : List (m a)) ->
      match ms with
      | Nil @(m a) -> pure$ll1 @m dict$Monad$m @(List a) (Nil @a)
      | Cons @(m a) m ms ->
        bind$ll1 @m dict$Monad$m @a @(List a) m (sequence$ll2 @a @m dict$Monad$m ms)
sub$ll1 : ∀a. Dict$Ring a -> a -> a -> a =
  fun @a ->
    fun (dict : Dict$Ring a) ->
      match dict with
      | Dict$Ring @a _ _ sub _ -> sub
toList : ∀a. Fix2 ListF a -> List a =
  fun @a ->
    compose$ll1 @(Fix2 ListF a) @(Fix (ListF a)) @(List a) (cata$ll1 @(List a) @(ListF a) (dict$Functor$ListF @a) (toList$ll1 @a)) (mono$ll1 @a @ListF dict$Bifunctor$ListF)
toList$ll1 : ∀a. ListF a (List a) -> List a =
  fun @a ->
    fun (x : ListF a (List a)) ->
      match x with
      | NilF @a @(List a) -> Nil @a
      | ConsF @a @(List a) y ys -> Cons @a y ys
traverse_$ll1 : ∀a m. Dict$Monad m -> (a -> m Unit) -> a -> m Unit -> m Unit =
  fun @a @m ->
    fun (dict$Monad$m : Dict$Monad m) (f : a -> m Unit) (x : a) (m : m Unit) ->
      semi$ll2 @Unit @m dict$Monad$m (f x) m
traverse_$ll2 : ∀a m t. Dict$Monad m -> Dict$Foldable t -> (a -> m Unit) -> t a -> m Unit =
  fun @a @m @t ->
    fun (dict$Monad$m : Dict$Monad m) (dict$Foldable$t : Dict$Foldable t) (f : a -> m Unit) ->
      foldr$ll1 @t dict$Foldable$t @a @(m Unit) (traverse_$ll1 @a @m dict$Monad$m f) (pure$ll1 @m dict$Monad$m @Unit Unit)
unFix$ll1 : ∀f. Fix f -> f (Fix f) =
  fun @f -> fun (x : Fix f) -> coerce @(Fix -> _) x
unFix2$ll1 : ∀a p. Fix2 p a -> p a (Fix2 p a) =
  fun @a @p -> fun (x : Fix2 p a) -> coerce @(Fix2 -> _) x
