external (-) : Int -> Int -> Int = "sub"
external (*) : Int -> Int -> Int = "mul"
external (<=) : Int -> Int -> Bool = "le"
let foldr : ∀a b. (a -> b -> b) -> b -> List a -> b =
      fun @a @b ->
        fun (f : a -> b -> b) (y0 : b) (xs : List a) ->
          match xs with
          | Nil @a -> y0
          | Cons @a x xs -> f x (foldr @a @b f y0 xs)
let replicate : ∀a. Int -> a -> List a =
      fun @a ->
        fun (n : Int) (x : a) ->
          match (<=) n 0 with
          | False -> Cons @a x (replicate @a ((-) n 1) x)
          | True -> Nil @a
external return : ∀a. a -> IO a = "return"
external print : Int -> IO Unit = "print"
external input : IO Int = "input"
external (>>=) : ∀a b. IO a -> (a -> IO b) -> IO b = "bind"
let (;ll1) : ∀a. IO a -> Unit -> IO a =
      fun @a -> fun (m2 : IO a) (x : Unit) -> m2
let (;) : ∀a. IO Unit -> IO a -> IO a =
      fun @a ->
        fun (m1 : IO Unit) (m2 : IO a) -> (>>=) @Unit @a m1 ((;ll1) @a m2)
let sequence_io$ll1 : ∀a. a -> List a -> IO (List a) =
      fun @a ->
        fun (x : a) (xs : List a) -> return @(List a) (Cons @a x xs)
let sequence_io$ll2 : ∀a. List (IO a) -> a -> IO (List a) =
      fun @a ->
        fun (ms : List (IO a)) (x : a) ->
          (>>=) @(List a) @(List a) (sequence_io @a ms) (sequence_io$ll1 @a x)
let sequence_io : ∀a. List (IO a) -> IO (List a) =
      fun @a ->
        fun (ms : List (IO a)) ->
          match ms with
          | Nil @(IO a) -> return @(List a) (Nil @a)
          | Cons @(IO a) m ms -> (>>=) @a @(List a) m (sequence_io$ll2 @a ms)
let iter_io$ll1 : ∀a. (a -> IO Unit) -> a -> IO Unit -> IO Unit =
      fun @a ->
        fun (f : a -> IO Unit) (x : a) (m : IO Unit) -> (;) @Unit (f x) m
let iter_io : ∀a. (a -> IO Unit) -> List a -> IO Unit =
      fun @a ->
        fun (f : a -> IO Unit) ->
          foldr @a @(IO Unit) (iter_io$ll1 @a f) (return @Unit Unit)
let id : ∀a. a -> a = fun @a -> fun (x : a) -> x
let cata : ∀a b. ((Fix b -> a) -> b (Fix b) -> b a) -> (b a -> a) -> Fix b -> a =
      fun @a @b ->
        fun (fmap : (Fix b -> a) -> b (Fix b) -> b a) (f : b a -> a) (x : Fix b) ->
          match x with
          | Fix @b y -> f (fmap (cata @a @b fmap f) y)
let ana : ∀a b. ((a -> Fix b) -> b a -> b (Fix b)) -> (a -> b a) -> a -> Fix b =
      fun @a @b ->
        fun (fmap : (a -> Fix b) -> b a -> b (Fix b)) (f : a -> b a) (x : a) ->
          Fix @b (fmap (ana @a @b fmap f) (f x))
let mapFix : ∀a b c. ((a -> b) -> (Fix (c a) -> Fix (c b)) -> c a (Fix (c a)) -> c b (Fix (c b))) -> (a -> b) -> Fix (c a) -> Fix (c b) =
      fun @a @b @c ->
        fun (bimap : (a -> b) -> (Fix (c a) -> Fix (c b)) -> c a (Fix (c a)) -> c b (Fix (c b))) (f : a -> b) (x : Fix (c a)) ->
          match x with
          | Fix @(c a) y -> Fix @(c b) (bimap f (mapFix @a @b @c bimap f) y)
let bimapListF : ∀a b c d. (a -> b) -> (c -> d) -> ListF a c -> ListF b d =
      fun @a @b @c @d ->
        fun (f : a -> b) (g : c -> d) (x : ListF a c) ->
          match x with
          | NilF @a @c -> NilF @b @d
          | ConsF @a @c y z -> ConsF @b @d (f y) (g z)
let mapFixList : ∀a b. (a -> b) -> Fix (ListF a) -> Fix (ListF b) =
      fun @a @b ->
        mapFix @a @b @ListF (bimapListF @a @b @(Fix (ListF a)) @(Fix (ListF b)))
let toList$ll1 : ∀a. ListF a (List a) -> List a =
      fun @a ->
        fun (x : ListF a (List a)) ->
          match x with
          | NilF @a @(List a) -> Nil @a
          | ConsF @a @(List a) y ys -> Cons @a y ys
let toList : ∀a. Fix (ListF a) -> List a =
      fun @a ->
        cata @(List a) @(ListF a) (bimapListF @a @a @(Fix (ListF a)) @(List a) (id @a)) (toList$ll1 @a)
let fromList$ll1 : ∀a. List a -> ListF a (List a) =
      fun @a ->
        fun (x : List a) ->
          match x with
          | Nil @a -> NilF @a @(List a)
          | Cons @a y ys -> ConsF @a @(List a) y ys
let fromList : ∀a. List a -> Fix (ListF a) =
      fun @a ->
        ana @(List a) @(ListF a) (bimapListF @a @a @(List a) @(Fix (ListF a)) (id @a)) (fromList$ll1 @a)
let main$ll1 : Int -> Int = fun (x : Int) -> (*) 2 x
let main$ll2 : List Int -> IO Unit =
      fun (xs : List Int) ->
        iter_io @Int print (toList @Int (mapFixList @Int @Int main$ll1 (fromList @Int xs)))
let main$ll3 : Int -> IO Unit =
      fun (n : Int) ->
        (>>=) @(List Int) @Unit (sequence_io @Int (replicate @(IO Int) n input)) main$ll2
let main : IO Unit = (>>=) @Int @Unit input main$ll3
