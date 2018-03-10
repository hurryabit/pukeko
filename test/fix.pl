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
data Fix f = f (Fix f)
data Bifunctor p =
       | Dict$Bifunctor (∀a1 a2 b1 b2. (a1 -> a2) -> (b1 -> b2) -> p a1 b1 -> p a2 b2)
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
dict$Ord$Int : Ord Int = Dict$Ord @Int ge_int gt_int le_int lt_int
dict$Ring$Int : Ring Int =
  Dict$Ring @Int neg_int add_int sub_int mul_int
dict$Foldable$List : Foldable List =
  Dict$Foldable @List dict$Foldable$List$ll1 dict$Foldable$List$ll2
dict$Monad$IO : Monad IO =
  Dict$Monad @IO dict$Monad$IO$ll2 dict$Monad$IO$ll4
input : IO Int = coerce @(_ -> IO) (io$ll1 @Unit @Int geti Unit)
dict$Bifunctor$ListF : Bifunctor ListF =
  Dict$Bifunctor @ListF dict$Bifunctor$ListF$ll1
main : IO Unit =
  (match dict$Monad$IO with
   | Dict$Monad _ bind -> bind) @Int @Unit input main$ll3
id$ll1 : ∀a. a -> a = fun @a (x : a) -> x
compose$ll1 : ∀a b c. (b -> c) -> (a -> b) -> a -> c =
  fun @a @b @c (f : b -> c) (g : a -> b) (x : a) -> f (g x)
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
replicate$ll1 : ∀a. Int -> a -> List a =
  fun @a (n : Int) (x : a) ->
    match (match dict$Ord$Int with
           | Dict$Ord _ _ le _ -> le) n 0 with
    | False ->
      Cons @a x (replicate$ll1 @a ((match dict$Ring$Int with
                                    | Dict$Ring _ _ sub _ -> sub) n 1) x)
    | True -> Nil @a
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
fix$ll1 : ∀f. f (Fix f) -> Fix f =
  fun @f (x : f (Fix f)) -> coerce @(_ -> Fix) x
unFix$ll1 : ∀f. Fix f -> f (Fix f) =
  fun @f (x : Fix f) -> coerce @(Fix -> _) x
cata$ll1 : ∀a f. Functor f -> (f a -> a) -> Fix f -> a =
  fun @a @f (dict$Functor$f : Functor f) (f : f a -> a) ->
    compose$ll1 @(Fix f) @(f a) @a f (compose$ll1 @(Fix f) @(f (Fix f)) @(f a) ((match dict$Functor$f with
                                                                                 | Dict$Functor map ->
                                                                                   map) @(Fix f) @a (cata$ll1 @a @f dict$Functor$f f)) (unFix$ll1 @f))
ana$ll1 : ∀a f. Functor f -> (a -> f a) -> a -> Fix f =
  fun @a @f (dict$Functor$f : Functor f) (f : a -> f a) ->
    compose$ll1 @a @(f (Fix f)) @(Fix f) (fix$ll1 @f) (compose$ll1 @a @(f a) @(f (Fix f)) ((match dict$Functor$f with
                                                                                            | Dict$Functor map ->
                                                                                              map) @a @(Fix f) (ana$ll1 @a @f dict$Functor$f f)) f)
fix2$ll1 : ∀a p. p a (Fix2 p a) -> Fix2 p a =
  fun @a @p (x : p a (Fix2 p a)) -> coerce @(_ -> Fix2) x
unFix2$ll1 : ∀a p. Fix2 p a -> p a (Fix2 p a) =
  fun @a @p (x : Fix2 p a) -> coerce @(Fix2 -> _) x
dict$Functor$Fix2$ll1 : ∀p. Bifunctor p -> (∀a b. (a -> b) -> Fix2 p a -> Fix2 p b) =
  fun @p (dict$Bifunctor$p : Bifunctor p) @a @b (f : a -> b) ->
    compose$ll1 @(Fix2 p a) @(p b (Fix2 p b)) @(Fix2 p b) (fix2$ll1 @b @p) (compose$ll1 @(Fix2 p a) @(p a (Fix2 p a)) @(p b (Fix2 p b)) ((match dict$Bifunctor$p with
                                                                                                                                          | Dict$Bifunctor bimap ->
                                                                                                                                            bimap) @a @b @(Fix2 p a) @(Fix2 p b) f (let dict : Functor (Fix2 p) =
                                                                                                                                                                                          dict$Functor$Fix2$ll2 @p dict$Bifunctor$p
                                                                                                                                                                                    in
                                                                                                                                                                                    (match dict with
                                                                                                                                                                                     | Dict$Functor map ->
                                                                                                                                                                                       map) @a @b f)) (unFix2$ll1 @a @p))
dict$Functor$Fix2$ll2 : ∀p. Bifunctor p -> Functor (Fix2 p) =
  fun @p (dict$Bifunctor$p : Bifunctor p) ->
    let map : ∀a b. (a -> b) -> Fix2 p a -> Fix2 p b =
          dict$Functor$Fix2$ll1 @p dict$Bifunctor$p
    in
    Dict$Functor @(Fix2 p) map
poly$ll1 : ∀a p. Bifunctor p -> Fix (p a) -> Fix2 p a =
  fun @a @p (dict$Bifunctor$p : Bifunctor p) ->
    compose$ll1 @(Fix (p a)) @(p a (Fix2 p a)) @(Fix2 p a) (fix2$ll1 @a @p) (compose$ll1 @(Fix (p a)) @(p a (Fix (p a))) @(p a (Fix2 p a)) ((match dict$Bifunctor$p with
                                                                                                                                             | Dict$Bifunctor bimap ->
                                                                                                                                               bimap) @a @a @(Fix (p a)) @(Fix2 p a) (id$ll1 @a) (poly$ll1 @a @p dict$Bifunctor$p)) (unFix$ll1 @(p a)))
mono$ll1 : ∀a p. Bifunctor p -> Fix2 p a -> Fix (p a) =
  fun @a @p (dict$Bifunctor$p : Bifunctor p) ->
    compose$ll1 @(Fix2 p a) @(p a (Fix (p a))) @(Fix (p a)) (fix$ll1 @(p a)) (compose$ll1 @(Fix2 p a) @(p a (Fix2 p a)) @(p a (Fix (p a))) ((match dict$Bifunctor$p with
                                                                                                                                             | Dict$Bifunctor bimap ->
                                                                                                                                               bimap) @a @a @(Fix2 p a) @(Fix (p a)) (id$ll1 @a) (mono$ll1 @a @p dict$Bifunctor$p)) (unFix2$ll1 @a @p))
dict$Functor$ListF$ll1 : ∀a a b. (a -> b) -> ListF a a -> ListF a b =
  fun @a @a @b ->
    (match dict$Bifunctor$ListF with
     | Dict$Bifunctor bimap -> bimap) @a @a @a @b (id$ll1 @a)
dict$Bifunctor$ListF$ll1 : ∀a1 a2 b1 b2. (a1 -> a2) -> (b1 -> b2) -> ListF a1 b1 -> ListF a2 b2 =
  fun @a1 @a2 @b1 @b2 (f : a1 -> a2) (g : b1 -> b2) (x : ListF a1 b1) ->
    match x with
    | NilF -> NilF @a2 @b2
    | ConsF y z -> ConsF @a2 @b2 (f y) (g z)
toList$ll1 : ∀a. ListF a (List a) -> List a =
  fun @a (x : ListF a (List a)) ->
    match x with
    | NilF -> Nil @a
    | ConsF y ys -> Cons @a y ys
fromList$ll1 : ∀a. List a -> ListF a (List a) =
  fun @a (x : List a) ->
    match x with
    | Nil -> NilF @a @(List a)
    | Cons y ys -> ConsF @a @(List a) y ys
main$ll1 : Int -> Int =
  (match dict$Ring$Int with
   | Dict$Ring _ _ _ mul -> mul) 2
main$ll2 : List Int -> IO Unit =
  fun (xs : List Int) ->
    (match dict$Foldable$List with
     | Dict$Foldable foldr _ ->
       foldr) @Int @(IO Unit) (traverse_$ll1 @Int @IO dict$Monad$IO print$ll1) ((match dict$Monad$IO with
                                                                                 | Dict$Monad pure _ ->
                                                                                   pure) @Unit Unit) (let f : Fix (ListF Int) -> List Int =
                                                                                                            cata$ll1 @(List Int) @(ListF Int) (let map : ∀a b. (a -> b) -> ListF Int a -> ListF Int b =
                                                                                                                                                     dict$Functor$ListF$ll1 @Int
                                                                                                                                               in
                                                                                                                                               Dict$Functor @(ListF Int) map) (toList$ll1 @Int)
                                                                                                      and g : Fix2 ListF Int -> Fix (ListF Int) =
                                                                                                            mono$ll1 @Int @ListF dict$Bifunctor$ListF
                                                                                                      and x : Fix2 ListF Int =
                                                                                                            let dict : Functor (Fix2 ListF) =
                                                                                                                  dict$Functor$Fix2$ll2 @ListF dict$Bifunctor$ListF
                                                                                                            in
                                                                                                            (match dict with
                                                                                                             | Dict$Functor map ->
                                                                                                               map) @Int @Int main$ll1 (let f : Fix (ListF Int) -> Fix2 ListF Int =
                                                                                                                                              poly$ll1 @Int @ListF dict$Bifunctor$ListF
                                                                                                                                        and g : List Int -> Fix (ListF Int) =
                                                                                                                                              ana$ll1 @(List Int) @(ListF Int) (let map : ∀a b. (a -> b) -> ListF Int a -> ListF Int b =
                                                                                                                                                                                      dict$Functor$ListF$ll1 @Int
                                                                                                                                                                                in
                                                                                                                                                                                Dict$Functor @(ListF Int) map) (fromList$ll1 @Int)
                                                                                                                                        in
                                                                                                                                        f (g xs))
                                                                                                      in
                                                                                                      f (g x))
main$ll3 : Int -> IO Unit =
  fun (n : Int) ->
    (match dict$Monad$IO with
     | Dict$Monad _ bind ->
       bind) @(List Int) @Unit (sequence$ll3 @Int @IO dict$Monad$IO (replicate$ll1 @(IO Int) n input)) main$ll2
