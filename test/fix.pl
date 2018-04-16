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
data World
data IO a = World -> Pair a World
data Fix f = f (Fix f)
data Bifunctor p =
       | .Bifunctor (∀a1 a2 b1 b2. (a1 -> a2) -> (b1 -> b2) -> p a1 b1 -> p a2 b2)
data Fix2 p a = p a (Fix2 p a)
data FixPoly p a = Fix (p a)
data ListF a b =
       | NilF
       | ConsF a b
external le_int : Int -> Int -> Bool = "le"
external sub_int : Int -> Int -> Int = "sub"
external mul_int : Int -> Int -> Int = "mul"
external seq : ∀a b. a -> b -> b = "seq"
external puti : Int -> Unit = "puti"
external geti : Unit -> Int = "geti"
monadIO : Monad IO = .Monad @IO monadIO.pure.L2 monadIO.bind.L2
print : Int -> IO Unit = io.L2 @Int @Unit puti
input : IO Int = coerce @(_ -> IO) (io.L1 @Unit @Int geti Unit)
functorFox2 : ∀p. Bifunctor p -> Functor (Fix2 p) =
  fun @p (bifunctor.p : Bifunctor p) ->
    .Functor @(Fix2 p) (functorFox2.map.L1 @p bifunctor.p)
poly : ∀a p. Bifunctor p -> Fix (p a) -> Fix2 p a =
  fun @a @p (bifunctor.p : Bifunctor p) ->
    compose.L1 @(Fix (p a)) @(p a (Fix2 p a)) @(Fix2 p a) (fix2.L1 @a @p) (compose.L1 @(Fix (p a)) @(p a (Fix (p a))) @(p a (Fix2 p a)) ((match bifunctor.p with
                                                                                                                                          | .Bifunctor bimap ->
                                                                                                                                            bimap) @a @a @(Fix (p a)) @(Fix2 p a) (id.L1 @a) (poly @a @p bifunctor.p)) (unFix.L1 @(p a)))
mono : ∀a p. Bifunctor p -> Fix2 p a -> Fix (p a) =
  fun @a @p (bifunctor.p : Bifunctor p) ->
    compose.L1 @(Fix2 p a) @(p a (Fix (p a))) @(Fix (p a)) (fix.L1 @(p a)) (compose.L1 @(Fix2 p a) @(p a (Fix2 p a)) @(p a (Fix (p a))) ((match bifunctor.p with
                                                                                                                                          | .Bifunctor bimap ->
                                                                                                                                            bimap) @a @a @(Fix2 p a) @(Fix (p a)) (id.L1 @a) (mono @a @p bifunctor.p)) (unFix2.L1 @a @p))
bifunctorListF : Bifunctor ListF =
  .Bifunctor @ListF bifunctorListF.bimap.L1
main : IO Unit =
  coerce @(_ -> IO) (monadIO.bind.L1 @Int @Unit input main.L3)
id.L1 : ∀a. a -> a = fun @a (x : a) -> x
compose.L1 : ∀a b c. (b -> c) -> (a -> b) -> a -> c =
  fun @a @b @c (f : b -> c) (g : a -> b) (x : a) -> f (g x)
foldableList.foldr.L1 : ∀a b. (a -> b -> b) -> b -> List a -> b =
  fun @a @b (f : a -> b -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> y0
    | Cons x xs -> f x (foldableList.foldr.L1 @a @b f y0 xs)
replicate.L1 : ∀a. Int -> a -> List a =
  fun @a (n : Int) (x : a) ->
    match le_int n 0 with
    | False -> Cons @a x (replicate.L1 @a (sub_int n 1) x)
    | True -> Nil @a
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
fix.L1 : ∀f. f (Fix f) -> Fix f =
  fun @f (x : f (Fix f)) -> coerce @(_ -> Fix) x
unFix.L1 : ∀f. Fix f -> f (Fix f) =
  fun @f (x : Fix f) -> coerce @(Fix -> _) x
cata.L1 : ∀a f. Functor f -> (f a -> a) -> Fix f -> a =
  fun @a @f (functor.f : Functor f) (f : f a -> a) ->
    compose.L1 @(Fix f) @(f a) @a f (compose.L1 @(Fix f) @(f (Fix f)) @(f a) ((match functor.f with
                                                                               | .Functor map ->
                                                                                 map) @(Fix f) @a (cata.L1 @a @f functor.f f)) (unFix.L1 @f))
ana.L1 : ∀a f. Functor f -> (a -> f a) -> a -> Fix f =
  fun @a @f (functor.f : Functor f) (f : a -> f a) ->
    compose.L1 @a @(f (Fix f)) @(Fix f) (fix.L1 @f) (compose.L1 @a @(f a) @(f (Fix f)) ((match functor.f with
                                                                                         | .Functor map ->
                                                                                           map) @a @(Fix f) (ana.L1 @a @f functor.f f)) f)
fix2.L1 : ∀a p. p a (Fix2 p a) -> Fix2 p a =
  fun @a @p (x : p a (Fix2 p a)) -> coerce @(_ -> Fix2) x
unFix2.L1 : ∀a p. Fix2 p a -> p a (Fix2 p a) =
  fun @a @p (x : Fix2 p a) -> coerce @(Fix2 -> _) x
functorFox2.map.L1 : ∀p. Bifunctor p -> (∀a b. (a -> b) -> Fix2 p a -> Fix2 p b) =
  fun @p (bifunctor.p : Bifunctor p) @a @b (f : a -> b) ->
    compose.L1 @(Fix2 p a) @(p b (Fix2 p b)) @(Fix2 p b) (fix2.L1 @b @p) (compose.L1 @(Fix2 p a) @(p a (Fix2 p a)) @(p b (Fix2 p b)) ((match bifunctor.p with
                                                                                                                                       | .Bifunctor bimap ->
                                                                                                                                         bimap) @a @b @(Fix2 p a) @(Fix2 p b) f (let dict : Functor (Fix2 p) =
                                                                                                                                                                                       functorFox2 @p bifunctor.p
                                                                                                                                                                                 in
                                                                                                                                                                                 (match dict with
                                                                                                                                                                                  | .Functor map ->
                                                                                                                                                                                    map) @a @b f)) (unFix2.L1 @a @p))
functorListF.map.L1 : ∀a a b. (a -> b) -> ListF a a -> ListF a b =
  fun @a @a @b -> bifunctorListF.bimap.L1 @a @a @a @b (id.L1 @a)
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
main.L1 : Int -> Int = mul_int 2
main.L2 : List Int -> IO Unit =
  fun (xs : List Int) ->
    foldableList.foldr.L1 @Int @(IO Unit) (traverse_.L1 @Int @IO monadIO print) (coerce @(_ -> IO) (Pair @Unit @World Unit)) (let f : Fix (ListF Int) -> List Int =
                                                                                                                                    cata.L1 @(List Int) @(ListF Int) (.Functor @(ListF Int) (functorListF.map.L1 @Int)) (toList.L1 @Int)
                                                                                                                              and g : Fix2 ListF Int -> Fix (ListF Int) =
                                                                                                                                    mono @Int @ListF bifunctorListF
                                                                                                                              and x : Fix2 ListF Int =
                                                                                                                                    let dict : Functor (Fix2 ListF) =
                                                                                                                                          functorFox2 @ListF bifunctorListF
                                                                                                                                    in
                                                                                                                                    (match dict with
                                                                                                                                     | .Functor map ->
                                                                                                                                       map) @Int @Int main.L1 (let f : Fix (ListF Int) -> Fix2 ListF Int =
                                                                                                                                                                     poly @Int @ListF bifunctorListF
                                                                                                                                                               and g : List Int -> Fix (ListF Int) =
                                                                                                                                                                     ana.L1 @(List Int) @(ListF Int) (.Functor @(ListF Int) (functorListF.map.L1 @Int)) (fromList.L1 @Int)
                                                                                                                                                               in
                                                                                                                                                               f (g xs))
                                                                                                                              in
                                                                                                                              f (g x))
main.L3 : Int -> IO Unit =
  fun (n : Int) ->
    let mx : IO (List Int) =
          sequence.L3 @Int @IO monadIO (replicate.L1 @(IO Int) n input)
    in
    coerce @(_ -> IO) (monadIO.bind.L1 @(List Int) @Unit mx main.L2)
