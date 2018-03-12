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
data Fix f = f (Fix f)
data Bifunctor p =
       | .Bifunctor (∀a1 a2 b1 b2. (a1 -> a2) -> (b1 -> b2) -> p a1 b1 -> p a2 b2)
data Fix2 p a = p a (Fix2 p a)
data FixPoly p a = Fix (p a)
data ListF a b =
       | NilF
       | ConsF a b
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
le : ∀a. Ord a -> a -> a -> Bool =
  fun @a (dict : Ord a) ->
    match dict with
    | .Ord _ _ le _ -> le
sub : ∀a. Ring a -> a -> a -> a =
  fun @a (dict : Ring a) ->
    match dict with
    | .Ring _ _ sub _ -> sub
mul : ∀a. Ring a -> a -> a -> a =
  fun @a (dict : Ring a) ->
    match dict with
    | .Ring _ _ _ mul -> mul
ordInt : Ord Int = .Ord @Int ge_int gt_int le_int lt_int
ringInt : Ring Int = .Ring @Int neg_int add_int sub_int mul_int
foldr : ∀t. Foldable t -> (∀a b. (a -> b -> b) -> b -> t a -> b) =
  fun @t (dict : Foldable t) ->
    match dict with
    | .Foldable foldr _ -> foldr
foldl : ∀t. Foldable t -> (∀a b. (b -> a -> b) -> b -> t a -> b) =
  fun @t (dict : Foldable t) ->
    match dict with
    | .Foldable _ foldl -> foldl
map : ∀f. Functor f -> (∀a b. (a -> b) -> f a -> f b) =
  fun @f (dict : Functor f) ->
    match dict with
    | .Functor map -> map
foldableList : Foldable List =
  .Foldable @List foldableList.foldr.L1 foldableList.foldl.L1
pure : ∀m. Monad m -> (∀a. a -> m a) =
  fun @m (dict : Monad m) ->
    match dict with
    | .Monad pure _ -> pure
bind : ∀m. Monad m -> (∀a b. m a -> (a -> m b) -> m b) =
  fun @m (dict : Monad m) ->
    match dict with
    | .Monad _ bind -> bind
monadIO : Monad IO = .Monad @IO monadIO.pure.L2 monadIO.bind.L2
print : Int -> IO Unit = io.L2 @Int @Unit puti
input : IO Int = io.L2 @Unit @Int geti Unit
bimap : ∀p. Bifunctor p -> (∀a1 a2 b1 b2. (a1 -> a2) -> (b1 -> b2) -> p a1 b1 -> p a2 b2) =
  fun @p (dict : Bifunctor p) ->
    match dict with
    | .Bifunctor bimap -> bimap
functorFox2 : ∀p. Bifunctor p -> Functor (Fix2 p) =
  fun @p (bifunctor.p : Bifunctor p) ->
    .Functor @(Fix2 p) (functorFox2.map.L1 @p bifunctor.p)
poly : ∀a p. Bifunctor p -> Fix (p a) -> Fix2 p a =
  fun @a @p (bifunctor.p : Bifunctor p) ->
    compose.L1 @(Fix (p a)) @(p a (Fix2 p a)) @(Fix2 p a) (fix2.L1 @a @p) (compose.L1 @(Fix (p a)) @(p a (Fix (p a))) @(p a (Fix2 p a)) (bimap @p bifunctor.p @a @a @(Fix (p a)) @(Fix2 p a) (id.L1 @a) (poly @a @p bifunctor.p)) (unFix.L1 @(p a)))
mono : ∀a p. Bifunctor p -> Fix2 p a -> Fix (p a) =
  fun @a @p (bifunctor.p : Bifunctor p) ->
    compose.L1 @(Fix2 p a) @(p a (Fix (p a))) @(Fix (p a)) (fix.L1 @(p a)) (compose.L1 @(Fix2 p a) @(p a (Fix2 p a)) @(p a (Fix (p a))) (bimap @p bifunctor.p @a @a @(Fix2 p a) @(Fix (p a)) (id.L1 @a) (mono @a @p bifunctor.p)) (unFix2.L1 @a @p))
functorListF : ∀a. Functor (ListF a) =
  fun @a -> .Functor @(ListF a) (functorListF.map.L1 @a)
bifunctorListF : Bifunctor ListF =
  .Bifunctor @ListF bifunctorListF.bimap.L1
toList : ∀a. Fix2 ListF a -> List a =
  fun @a ->
    compose.L1 @(Fix2 ListF a) @(Fix (ListF a)) @(List a) (cata.L1 @(List a) @(ListF a) (functorListF @a) (toList.L1 @a)) (mono @a @ListF bifunctorListF)
fromList : ∀a. List a -> Fix2 ListF a =
  fun @a ->
    compose.L1 @(List a) @(Fix (ListF a)) @(Fix2 ListF a) (poly @a @ListF bifunctorListF) (ana.L1 @(List a) @(ListF a) (functorListF @a) (fromList.L1 @a))
main : IO Unit = bind @IO monadIO @Int @Unit input main.L3
id.L1 : ∀a. a -> a = fun @a (x : a) -> x
compose.L1 : ∀a b c. (b -> c) -> (a -> b) -> a -> c =
  fun @a @b @c (f : b -> c) (g : a -> b) (x : a) -> f (g x)
foldableList.foldr.L1 : ∀a b. (a -> b -> b) -> b -> List a -> b =
  fun @a @b (f : a -> b -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> y0
    | Cons x xs -> f x (foldr @List foldableList @a @b f y0 xs)
foldableList.foldl.L1 : ∀a b. (b -> a -> b) -> b -> List a -> b =
  fun @a @b (f : b -> a -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> y0
    | Cons x xs -> foldl @List foldableList @a @b f (f y0 x) xs
replicate.L1 : ∀a. Int -> a -> List a =
  fun @a (n : Int) (x : a) ->
    match le @Int ordInt n 0 with
    | False -> Cons @a x (replicate.L1 @a (sub @Int ringInt n 1) x)
    | True -> Nil @a
semi.L1 : ∀a m. m a -> Unit -> m a =
  fun @a @m (m2 : m a) (x : Unit) -> m2
semi.L2 : ∀a m. Monad m -> m Unit -> m a -> m a =
  fun @a @m (monad.m : Monad m) (m1 : m Unit) (m2 : m a) ->
    bind @m monad.m @Unit @a m1 (semi.L1 @a @m m2)
sequence.L1 : ∀a m. Monad m -> a -> List a -> m (List a) =
  fun @a @m (monad.m : Monad m) (x : a) (xs : List a) ->
    pure @m monad.m @(List a) (Cons @a x xs)
sequence.L2 : ∀a m. Monad m -> List (m a) -> a -> m (List a) =
  fun @a @m (monad.m : Monad m) (ms : List (m a)) (x : a) ->
    bind @m monad.m @(List a) @(List a) (sequence.L3 @a @m monad.m ms) (sequence.L1 @a @m monad.m x)
sequence.L3 : ∀a m. Monad m -> List (m a) -> m (List a) =
  fun @a @m (monad.m : Monad m) (ms : List (m a)) ->
    match ms with
    | Nil -> pure @m monad.m @(List a) (Nil @a)
    | Cons m ms ->
      bind @m monad.m @a @(List a) m (sequence.L2 @a @m monad.m ms)
traverse_.L1 : ∀a m. Monad m -> (a -> m Unit) -> a -> m Unit -> m Unit =
  fun @a @m (monad.m : Monad m) (f : a -> m Unit) (x : a) ->
    semi.L2 @Unit @m monad.m (f x)
traverse_.L2 : ∀a m t. Monad m -> Foldable t -> (a -> m Unit) -> t a -> m Unit =
  fun @a @m @t (monad.m : Monad m) (foldable.t : Foldable t) (f : a -> m Unit) ->
    foldr @t foldable.t @a @(m Unit) (traverse_.L1 @a @m monad.m f) (pure @m monad.m @Unit Unit)
monadIO.pure.L1 : ∀a. a -> World -> Pair a World =
  fun @a -> Pair @a @World
monadIO.pure.L2 : ∀a. a -> IO a =
  fun @a (x : a) -> coerce @(_ -> IO) (monadIO.pure.L1 @a x)
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
fix.L1 : ∀f. f (Fix f) -> Fix f =
  fun @f (x : f (Fix f)) -> coerce @(_ -> Fix) x
unFix.L1 : ∀f. Fix f -> f (Fix f) =
  fun @f (x : Fix f) -> coerce @(Fix -> _) x
cata.L1 : ∀a f. Functor f -> (f a -> a) -> Fix f -> a =
  fun @a @f (functor.f : Functor f) (f : f a -> a) ->
    compose.L1 @(Fix f) @(f a) @a f (compose.L1 @(Fix f) @(f (Fix f)) @(f a) (map @f functor.f @(Fix f) @a (cata.L1 @a @f functor.f f)) (unFix.L1 @f))
ana.L1 : ∀a f. Functor f -> (a -> f a) -> a -> Fix f =
  fun @a @f (functor.f : Functor f) (f : a -> f a) ->
    compose.L1 @a @(f (Fix f)) @(Fix f) (fix.L1 @f) (compose.L1 @a @(f a) @(f (Fix f)) (map @f functor.f @a @(Fix f) (ana.L1 @a @f functor.f f)) f)
fix2.L1 : ∀a p. p a (Fix2 p a) -> Fix2 p a =
  fun @a @p (x : p a (Fix2 p a)) -> coerce @(_ -> Fix2) x
unFix2.L1 : ∀a p. Fix2 p a -> p a (Fix2 p a) =
  fun @a @p (x : Fix2 p a) -> coerce @(Fix2 -> _) x
functorFox2.map.L1 : ∀p. Bifunctor p -> (∀a b. (a -> b) -> Fix2 p a -> Fix2 p b) =
  fun @p (bifunctor.p : Bifunctor p) @a @b (f : a -> b) ->
    compose.L1 @(Fix2 p a) @(p b (Fix2 p b)) @(Fix2 p b) (fix2.L1 @b @p) (compose.L1 @(Fix2 p a) @(p a (Fix2 p a)) @(p b (Fix2 p b)) (bimap @p bifunctor.p @a @b @(Fix2 p a) @(Fix2 p b) f (map @(Fix2 p) (functorFox2 @p bifunctor.p) @a @b f)) (unFix2.L1 @a @p))
functorListF.map.L1 : ∀a a b. (a -> b) -> ListF a a -> ListF a b =
  fun @a @a @b -> bimap @ListF bifunctorListF @a @a @a @b (id.L1 @a)
bifunctorListF.bimap.L1 : ∀a1 a2 b1 b2. (a1 -> a2) -> (b1 -> b2) -> ListF a1 b1 -> ListF a2 b2 =
  fun @a1 @a2 @b1 @b2 (f : a1 -> a2) (g : b1 -> b2) (x : ListF a1 b1) ->
    match x with
    | NilF -> NilF @a2 @b2
    | ConsF y z -> ConsF @a2 @b2 (f y) (g z)
toList.L1 : ∀a. ListF a (List a) -> List a =
  fun @a (x : ListF a (List a)) ->
    match x with
    | NilF -> Nil @a
    | ConsF y ys -> Cons @a y ys
fromList.L1 : ∀a. List a -> ListF a (List a) =
  fun @a (x : List a) ->
    match x with
    | Nil -> NilF @a @(List a)
    | Cons y ys -> ConsF @a @(List a) y ys
main.L1 : Int -> Int = mul @Int ringInt 2
main.L2 : List Int -> IO Unit =
  fun (xs : List Int) ->
    traverse_.L2 @Int @IO @List monadIO foldableList print (toList @Int (map @(Fix2 ListF) (functorFox2 @ListF bifunctorListF) @Int @Int main.L1 (fromList @Int xs)))
main.L3 : Int -> IO Unit =
  fun (n : Int) ->
    bind @IO monadIO @(List Int) @Unit (sequence.L3 @Int @IO monadIO (replicate.L1 @(IO Int) n input)) main.L2
