type Unit =
       | Unit
type Pair a b =
       | Pair a b
type Bool =
       | False
       | True
type Choice a b =
       | First a
       | Second b
type Int
external (-) : Int -> Int -> Int = "sub"
external (*) : Int -> Int -> Int = "mul"
external (<=) : Int -> Int -> Bool = "le"
type List a =
       | Nil
       | Cons a (List a)
foldr : ∀a b. (a -> b -> b) -> b -> List a -> b =
  fun @a @b ->
    fun (f : a -> b -> b) (y0 : b) (xs : List a) ->
      match xs with
      | Nil @a -> y0
      | Cons @a x xs -> f x (foldr @a @b f y0 xs)
replicate : ∀a. Int -> a -> List a =
  fun @a ->
    fun (n : Int) (x : a) ->
      match (<=) n 0 with
      | False -> Cons @a x (replicate @a ((-) n 1) x)
      | True -> Nil @a
type IO a
external return : ∀a. a -> IO a = "return"
external print : Int -> IO Unit = "print"
external input : IO Int = "input"
external (>>=) : ∀a b. IO a -> (a -> IO b) -> IO b = "bind"
(;ll1) : ∀a. IO a -> Unit -> IO a =
  fun @a -> fun (m2 : IO a) (x : Unit) -> m2
(;) : ∀a. IO Unit -> IO a -> IO a =
  fun @a ->
    fun (m1 : IO Unit) (m2 : IO a) -> (>>=) @Unit @a m1 ((;ll1) @a m2)
sequence_io$ll1 : ∀a. a -> List a -> IO (List a) =
  fun @a ->
    fun (x : a) (xs : List a) -> return @(List a) (Cons @a x xs)
sequence_io$ll2 : ∀a. List (IO a) -> a -> IO (List a) =
  fun @a ->
    fun (ms : List (IO a)) (x : a) ->
      (>>=) @(List a) @(List a) (sequence_io @a ms) (sequence_io$ll1 @a x)
sequence_io : ∀a. List (IO a) -> IO (List a) =
  fun @a ->
    fun (ms : List (IO a)) ->
      match ms with
      | Nil @(IO a) -> return @(List a) (Nil @a)
      | Cons @(IO a) m ms -> (>>=) @a @(List a) m (sequence_io$ll2 @a ms)
iter_io$ll1 : ∀a. (a -> IO Unit) -> a -> IO Unit -> IO Unit =
  fun @a ->
    fun (f : a -> IO Unit) (x : a) (m : IO Unit) -> (;) @Unit (f x) m
iter_io : ∀a. (a -> IO Unit) -> List a -> IO Unit =
  fun @a ->
    fun (f : a -> IO Unit) ->
      foldr @a @(IO Unit) (iter_io$ll1 @a f) (return @Unit Unit)
type Option a =
       | None
       | Some a
id : ∀a. a -> a = fun @a -> fun (x : a) -> x
type Fix f =
       | Fix (f (Fix f))
cata : ∀a f. ((Fix f -> a) -> f (Fix f) -> f a) -> (f a -> a) -> Fix f -> a =
  fun @a @f ->
    fun (fmap : (Fix f -> a) -> f (Fix f) -> f a) (f : f a -> a) (x : Fix f) ->
      match x with
      | Fix @f y -> f (fmap (cata @a @f fmap f) y)
ana : ∀a f. ((a -> Fix f) -> f a -> f (Fix f)) -> (a -> f a) -> a -> Fix f =
  fun @a @f ->
    fun (fmap : (a -> Fix f) -> f a -> f (Fix f)) (f : a -> f a) (x : a) ->
      Fix @f (fmap (ana @a @f fmap f) (f x))
mapFix : ∀a b f. ((a -> b) -> (Fix (f a) -> Fix (f b)) -> f a (Fix (f a)) -> f b (Fix (f b))) -> (a -> b) -> Fix (f a) -> Fix (f b) =
  fun @a @b @f ->
    fun (bimap : (a -> b) -> (Fix (f a) -> Fix (f b)) -> f a (Fix (f a)) -> f b (Fix (f b))) (f : a -> b) (x : Fix (f a)) ->
      match x with
      | Fix @(f a) y -> Fix @(f b) (bimap f (mapFix @a @b @f bimap f) y)
type ListF a b =
       | NilF
       | ConsF a b
bimapListF : ∀a1 a2 b1 b2. (a1 -> a2) -> (b1 -> b2) -> ListF a1 b1 -> ListF a2 b2 =
  fun @a1 @a2 @b1 @b2 ->
    fun (f : a1 -> a2) (g : b1 -> b2) (x : ListF a1 b1) ->
      match x with
      | NilF @a1 @b1 -> NilF @a2 @b2
      | ConsF @a1 @b1 y z -> ConsF @a2 @b2 (f y) (g z)
mapFixList : ∀a b. (a -> b) -> Fix (ListF a) -> Fix (ListF b) =
  fun @a @b ->
    mapFix @a @b @ListF (bimapListF @a @b @(Fix (ListF a)) @(Fix (ListF b)))
toList$ll1 : ∀a. ListF a (List a) -> List a =
  fun @a ->
    fun (x : ListF a (List a)) ->
      match x with
      | NilF @a @(List a) -> Nil @a
      | ConsF @a @(List a) y ys -> Cons @a y ys
toList : ∀a. Fix (ListF a) -> List a =
  fun @a ->
    cata @(List a) @(ListF a) (bimapListF @a @a @(Fix (ListF a)) @(List a) (id @a)) (toList$ll1 @a)
fromList$ll1 : ∀a. List a -> ListF a (List a) =
  fun @a ->
    fun (x : List a) ->
      match x with
      | Nil @a -> NilF @a @(List a)
      | Cons @a y ys -> ConsF @a @(List a) y ys
fromList : ∀a. List a -> Fix (ListF a) =
  fun @a ->
    ana @(List a) @(ListF a) (bimapListF @a @a @(List a) @(Fix (ListF a)) (id @a)) (fromList$ll1 @a)
main$ll1 : Int -> Int = fun (x : Int) -> (*) 2 x
main$ll2 : List Int -> IO Unit =
  fun (xs : List Int) ->
    iter_io @Int print (toList @Int (mapFixList @Int @Int main$ll1 (fromList @Int xs)))
main$ll3 : Int -> IO Unit =
  fun (n : Int) ->
    (>>=) @(List Int) @Unit (sequence_io @Int (replicate @(IO Int) n input)) main$ll2
main : IO Unit = (>>=) @Int @Unit input main$ll3
