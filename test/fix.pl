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
data Dict$Eq a =
       | Dict$Eq (a -> a -> Bool)
id : ∀a. a -> a = fun @a -> fun (x : a) -> x
(∘) : ∀a b c. (b -> c) -> (a -> b) -> a -> c =
  fun @a @b @c -> fun (f : b -> c) (g : a -> b) (x : a) -> f (g x)
data Dict$Ord a =
       | Dict$Ord (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool)
(<=) : ∀a. Dict$Ord a -> a -> a -> Bool =
  fun @a ->
    fun (dict : Dict$Ord a) ->
      match dict with
      | Dict$Ord @a (<) (<=) (>=) (>) -> (<=)
data Dict$Monoid m =
       | Dict$Monoid m (m -> m -> m)
data Dict$Ring a =
       | Dict$Ring (a -> a) (a -> a -> a) (a -> a -> a) (a -> a -> a)
(-) : ∀a. Dict$Ring a -> a -> a -> a =
  fun @a ->
    fun (dict : Dict$Ring a) ->
      match dict with
      | Dict$Ring @a neg (+) (-) (*) -> (-)
(*) : ∀a. Dict$Ring a -> a -> a -> a =
  fun @a ->
    fun (dict : Dict$Ring a) ->
      match dict with
      | Dict$Ring @a neg (+) (-) (*) -> (*)
data Int
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
data Char
data Dict$Foldable t =
       | Dict$Foldable (∀a b. (a -> b -> b) -> b -> t a -> b) (∀a b. (b -> a -> b) -> b -> t a -> b)
foldr : ∀t. Dict$Foldable t -> (∀a b. (a -> b -> b) -> b -> t a -> b) =
  fun @t ->
    fun (dict : Dict$Foldable t) ->
      match dict with
      | Dict$Foldable @t foldr foldl -> foldr
foldl : ∀t. Dict$Foldable t -> (∀a b. (b -> a -> b) -> b -> t a -> b) =
  fun @t ->
    fun (dict : Dict$Foldable t) ->
      match dict with
      | Dict$Foldable @t foldr foldl -> foldl
data Dict$Functor f =
       | Dict$Functor (∀a b. (a -> b) -> f a -> f b)
map : ∀f. Dict$Functor f -> (∀a b. (a -> b) -> f a -> f b) =
  fun @f ->
    fun (dict : Dict$Functor f) ->
      match dict with
      | Dict$Functor @f map -> map
data List a =
       | Nil
       | Cons a (List a)
dict$Foldable$List$ll1 : ∀a b. (a -> b -> b) -> b -> List a -> b =
  fun @a @b ->
    fun (f : a -> b -> b) (y0 : b) (xs : List a) ->
      match xs with
      | Nil @a -> y0
      | Cons @a x xs ->
        f x (foldr @List dict$Foldable$List @a @b f y0 xs)
dict$Foldable$List$ll2 : ∀a b. (b -> a -> b) -> b -> List a -> b =
  fun @a @b ->
    fun (f : b -> a -> b) (y0 : b) (xs : List a) ->
      match xs with
      | Nil @a -> y0
      | Cons @a x xs ->
        foldl @List dict$Foldable$List @a @b f (f y0 x) xs
dict$Foldable$List : Dict$Foldable List =
  let foldr : ∀a b. (a -> b -> b) -> b -> List a -> b =
        fun @a @b -> dict$Foldable$List$ll1 @a @b
  and foldl : ∀a b. (b -> a -> b) -> b -> List a -> b =
        fun @a @b -> dict$Foldable$List$ll2 @a @b
  in
  Dict$Foldable @List foldr foldl
replicate : ∀a. Int -> a -> List a =
  fun @a ->
    fun (n : Int) (x : a) ->
      match (<=) @Int dict$Ord$Int n 0 with
      | False -> Cons @a x (replicate @a ((-) @Int dict$Ring$Int n 1) x)
      | True -> Nil @a
data Dict$Monad m =
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
(;ll1) : ∀a m. m a -> Unit -> m a =
  fun @a @m -> fun (m2 : m a) (x : Unit) -> m2
(;) : ∀a m. Dict$Monad m -> m Unit -> m a -> m a =
  fun @a @m ->
    fun (dict$Monad$m : Dict$Monad m) (m1 : m Unit) (m2 : m a) ->
      (>>=) @m dict$Monad$m @Unit @a m1 ((;ll1) @a @m m2)
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
traverse_$ll1 : ∀a m. Dict$Monad m -> (a -> m Unit) -> a -> m Unit -> m Unit =
  fun @a @m ->
    fun (dict$Monad$m : Dict$Monad m) (f : a -> m Unit) (x : a) (m : m Unit) ->
      (;) @Unit @m dict$Monad$m (f x) m
traverse_ : ∀a m t. Dict$Monad m -> Dict$Foldable t -> (a -> m Unit) -> t a -> m Unit =
  fun @a @m @t ->
    fun (dict$Monad$m : Dict$Monad m) (dict$Foldable$t : Dict$Foldable t) (f : a -> m Unit) ->
      foldr @t dict$Foldable$t @a @(m Unit) (traverse_$ll1 @a @m dict$Monad$m f) (pure @m dict$Monad$m @Unit Unit)
data IO a
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
data Fix f = f (Fix f)
fix : ∀f. f (Fix f) -> Fix f =
  fun @f -> fun (x : f (Fix f)) -> coerce @(f (Fix f) -> Fix f) x
unFix : ∀f. Fix f -> f (Fix f) =
  fun @f -> fun (x : Fix f) -> coerce @(Fix f -> f (Fix f)) x
cata : ∀a f. Dict$Functor f -> (f a -> a) -> Fix f -> a =
  fun @a @f ->
    fun (dict$Functor$f : Dict$Functor f) (f : f a -> a) ->
      (∘) @(Fix f) @(f a) @a f ((∘) @(Fix f) @(f (Fix f)) @(f a) (map @f dict$Functor$f @(Fix f) @a (cata @a @f dict$Functor$f f)) (unFix @f))
ana : ∀a f. Dict$Functor f -> (a -> f a) -> a -> Fix f =
  fun @a @f ->
    fun (dict$Functor$f : Dict$Functor f) (f : a -> f a) ->
      (∘) @a @(f (Fix f)) @(Fix f) (fix @f) ((∘) @a @(f a) @(f (Fix f)) (map @f dict$Functor$f @a @(Fix f) (ana @a @f dict$Functor$f f)) f)
data Dict$Bifunctor p =
       | Dict$Bifunctor (∀a1 a2 b1 b2. (a1 -> a2) -> (b1 -> b2) -> p a1 b1 -> p a2 b2)
bimap : ∀p. Dict$Bifunctor p -> (∀a1 a2 b1 b2. (a1 -> a2) -> (b1 -> b2) -> p a1 b1 -> p a2 b2) =
  fun @p ->
    fun (dict : Dict$Bifunctor p) ->
      match dict with
      | Dict$Bifunctor @p bimap -> bimap
data Fix2 p a = p a (Fix2 p a)
fix2 : ∀a p. p a (Fix2 p a) -> Fix2 p a =
  fun @a @p ->
    fun (x : p a (Fix2 p a)) -> coerce @(p a (Fix2 p a) -> Fix2 p a) x
unFix2 : ∀a p. Fix2 p a -> p a (Fix2 p a) =
  fun @a @p ->
    fun (x : Fix2 p a) -> coerce @(Fix2 p a -> p a (Fix2 p a)) x
dict$Functor$Fix2$ll1 : ∀p a b. Dict$Bifunctor p -> (a -> b) -> Fix2 p a -> Fix2 p b =
  fun @p @a @b ->
    fun (dict$Bifunctor$p : Dict$Bifunctor p) (f : a -> b) ->
      (∘) @(Fix2 p a) @(p b (Fix2 p b)) @(Fix2 p b) (fix2 @b @p) ((∘) @(Fix2 p a) @(p a (Fix2 p a)) @(p b (Fix2 p b)) (bimap @p dict$Bifunctor$p @a @b @(Fix2 p a) @(Fix2 p b) f (map @(Fix2 p) (dict$Functor$Fix2 @p dict$Bifunctor$p) @a @b f)) (unFix2 @a @p))
dict$Functor$Fix2 : ∀p. Dict$Bifunctor p -> Dict$Functor (Fix2 p) =
  fun @p ->
    fun (dict$Bifunctor$p : Dict$Bifunctor p) ->
      let map : ∀a b. (a -> b) -> Fix2 p a -> Fix2 p b =
            fun @a @b -> dict$Functor$Fix2$ll1 @p @a @b dict$Bifunctor$p
      in
      Dict$Functor @(Fix2 p) map
poly : ∀a p. Dict$Bifunctor p -> Fix (p a) -> Fix2 p a =
  fun @a @p ->
    fun (dict$Bifunctor$p : Dict$Bifunctor p) ->
      (∘) @(Fix (p a)) @(p a (Fix2 p a)) @(Fix2 p a) (fix2 @a @p) ((∘) @(Fix (p a)) @(p a (Fix (p a))) @(p a (Fix2 p a)) (bimap @p dict$Bifunctor$p @a @a @(Fix (p a)) @(Fix2 p a) (id @a) (poly @a @p dict$Bifunctor$p)) (unFix @(p a)))
mono : ∀a p. Dict$Bifunctor p -> Fix2 p a -> Fix (p a) =
  fun @a @p ->
    fun (dict$Bifunctor$p : Dict$Bifunctor p) ->
      (∘) @(Fix2 p a) @(p a (Fix (p a))) @(Fix (p a)) (fix @(p a)) ((∘) @(Fix2 p a) @(p a (Fix2 p a)) @(p a (Fix (p a))) (bimap @p dict$Bifunctor$p @a @a @(Fix2 p a) @(Fix (p a)) (id @a) (mono @a @p dict$Bifunctor$p)) (unFix2 @a @p))
data FixPoly p a = Fix (p a)
data ListF a b =
       | NilF
       | ConsF a b
dict$Functor$ListF : ∀a. Dict$Functor (ListF a) =
  fun @a ->
    let map : ∀b c. (b -> c) -> ListF a b -> ListF a c =
          fun @b @c -> bimap @ListF dict$Bifunctor$ListF @a @a @b @c (id @a)
    in
    Dict$Functor @(ListF a) map
dict$Bifunctor$ListF$ll1 : ∀a b c d. (a -> b) -> (c -> d) -> ListF a c -> ListF b d =
  fun @a @b @c @d ->
    fun (f : a -> b) (g : c -> d) (x : ListF a c) ->
      match x with
      | NilF @a @c -> NilF @b @d
      | ConsF @a @c y z -> ConsF @b @d (f y) (g z)
dict$Bifunctor$ListF : Dict$Bifunctor ListF =
  let bimap : ∀a b c d. (a -> b) -> (c -> d) -> ListF a c -> ListF b d =
        fun @a @b @c @d -> dict$Bifunctor$ListF$ll1 @a @b @c @d
  in
  Dict$Bifunctor @ListF bimap
toList$ll1 : ∀a. ListF a (List a) -> List a =
  fun @a ->
    fun (x : ListF a (List a)) ->
      match x with
      | NilF @a @(List a) -> Nil @a
      | ConsF @a @(List a) y ys -> Cons @a y ys
toList : ∀a. Fix2 ListF a -> List a =
  fun @a ->
    (∘) @(Fix2 ListF a) @(Fix (ListF a)) @(List a) (cata @(List a) @(ListF a) (dict$Functor$ListF @a) (toList$ll1 @a)) (mono @a @ListF dict$Bifunctor$ListF)
fromList$ll1 : ∀a. List a -> ListF a (List a) =
  fun @a ->
    fun (x : List a) ->
      match x with
      | Nil @a -> NilF @a @(List a)
      | Cons @a y ys -> ConsF @a @(List a) y ys
fromList : ∀a. List a -> Fix2 ListF a =
  fun @a ->
    (∘) @(List a) @(Fix (ListF a)) @(Fix2 ListF a) (poly @a @ListF dict$Bifunctor$ListF) (ana @(List a) @(ListF a) (dict$Functor$ListF @a) (fromList$ll1 @a))
main$ll1 : Int -> Int = fun (x : Int) -> (*) @Int dict$Ring$Int 2 x
main$ll2 : List Int -> IO Unit =
  fun (xs : List Int) ->
    traverse_ @Int @IO @List dict$Monad$IO dict$Foldable$List print (toList @Int (map @(Fix2 ListF) (dict$Functor$Fix2 @ListF dict$Bifunctor$ListF) @Int @Int main$ll1 (fromList @Int xs)))
main$ll3 : Int -> IO Unit =
  fun (n : Int) ->
    (>>=) @IO dict$Monad$IO @(List Int) @Unit (sequence @Int @IO dict$Monad$IO (replicate @(IO Int) n input)) main$ll2
main : IO Unit = (>>=) @IO dict$Monad$IO @Int @Unit input main$ll3
