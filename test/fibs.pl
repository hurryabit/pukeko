external abort : ∀a. a = "abort"
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
data Dict$Ord a =
       | Dict$Ord (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool)
le : ∀a. Dict$Ord a -> a -> a -> Bool =
  fun @a ->
    fun (dict : Dict$Ord a) ->
      match dict with
      | Dict$Ord @a _ _ le _ -> le
lt : ∀a. Dict$Ord a -> a -> a -> Bool =
  fun @a ->
    fun (dict : Dict$Ord a) ->
      match dict with
      | Dict$Ord @a _ _ _ lt -> lt
data Dict$Monoid m =
       | Dict$Monoid m (m -> m -> m)
data Dict$Ring a =
       | Dict$Ring (a -> a) (a -> a -> a) (a -> a -> a) (a -> a -> a)
add : ∀a. Dict$Ring a -> a -> a -> a =
  fun @a ->
    fun (dict : Dict$Ring a) ->
      match dict with
      | Dict$Ring @a _ add _ _ -> add
sub : ∀a. Dict$Ring a -> a -> a -> a =
  fun @a ->
    fun (dict : Dict$Ring a) ->
      match dict with
      | Dict$Ring @a _ _ sub _ -> sub
mul : ∀a. Dict$Ring a -> a -> a -> a =
  fun @a ->
    fun (dict : Dict$Ring a) ->
      match dict with
      | Dict$Ring @a _ _ _ mul -> mul
data Int
external lt_int : Int -> Int -> Bool = "lt"
external le_int : Int -> Int -> Bool = "le"
external ge_int : Int -> Int -> Bool = "ge"
external gt_int : Int -> Int -> Bool = "gt"
dict$Ord$Int : Dict$Ord Int =
  let ge : Int -> Int -> Bool = ge_int
  and gt : Int -> Int -> Bool = gt_int
  and le : Int -> Int -> Bool = le_int
  and lt : Int -> Int -> Bool = lt_int
  in
  Dict$Ord @Int ge gt le lt
external neg_int : Int -> Int = "neg"
external add_int : Int -> Int -> Int = "add"
external sub_int : Int -> Int -> Int = "sub"
external mul_int : Int -> Int -> Int = "mul"
dict$Ring$Int : Dict$Ring Int =
  let neg : Int -> Int = neg_int
  and add : Int -> Int -> Int = add_int
  and sub : Int -> Int -> Int = sub_int
  and mul : Int -> Int -> Int = mul_int
  in
  Dict$Ring @Int neg add sub mul
data Char
data Dict$Foldable t =
       | Dict$Foldable (∀a b. (a -> b -> b) -> b -> t a -> b) (∀a b. (b -> a -> b) -> b -> t a -> b)
data Dict$Functor f =
       | Dict$Functor (∀a b. (a -> b) -> f a -> f b)
data List a =
       | Nil
       | Cons a (List a)
nth_exn : ∀a. List a -> Int -> a =
  fun @a ->
    fun (xs : List a) (n : Int) ->
      match xs with
      | Nil @a -> abort @a
      | Cons @a x xs ->
        match le @Int dict$Ord$Int n 0 with
        | False -> nth_exn @a xs (sub @Int dict$Ring$Int n 1)
        | True -> x
zip_with : ∀a b c. (a -> b -> c) -> List a -> List b -> List c =
  fun @a @b @c ->
    fun (f : a -> b -> c) (xs : List a) (ys : List b) ->
      match xs with
      | Nil @a -> Nil @c
      | Cons @a x xs ->
        match ys with
        | Nil @b -> Nil @c
        | Cons @b y ys -> Cons @c (f x y) (zip_with @a @b @c f xs ys)
data Dict$Monad m =
       | Dict$Monad (∀a. a -> m a) (∀a b. m a -> (a -> m b) -> m b)
bind : ∀m. Dict$Monad m -> (∀a b. m a -> (a -> m b) -> m b) =
  fun @m ->
    fun (dict : Dict$Monad m) ->
      match dict with
      | Dict$Monad @m _ bind -> bind
external seq : ∀a b. a -> b -> b = "seq"
external puti : Int -> Unit = "puti"
external geti : Unit -> Int = "geti"
data World =
       | World
data IO a = World -> Pair a World
dict$Monad$IO$ll1 : ∀a. a -> World -> Pair a World =
  fun @a -> Pair @a @World
dict$Monad$IO$ll2 : ∀a. a -> IO a =
  fun @a ->
    fun (x : a) ->
      coerce @((World -> Pair a World) -> IO a) (dict$Monad$IO$ll1 @a x)
dict$Monad$IO$ll3 : ∀a b. IO a -> (a -> IO b) -> World -> Pair b World =
  fun @a @b ->
    fun (mx : IO a) (f : a -> IO b) (world0 : World) ->
      match coerce @(IO a -> World -> Pair a World) mx world0 with
      | Pair @a @World x world1 ->
        coerce @(IO b -> World -> Pair b World) (f x) world1
dict$Monad$IO$ll4 : ∀a b. IO a -> (a -> IO b) -> IO b =
  fun @a @b ->
    fun (mx : IO a) (f : a -> IO b) ->
      coerce @((World -> Pair b World) -> IO b) (dict$Monad$IO$ll3 @a @b mx f)
dict$Monad$IO : Dict$Monad IO =
  let pure : ∀a. a -> IO a = fun @a -> dict$Monad$IO$ll2 @a
  and bind : ∀a b. IO a -> (a -> IO b) -> IO b =
        fun @a @b -> dict$Monad$IO$ll4 @a @b
  in
  Dict$Monad @IO pure bind
io$ll1 : ∀a b. (a -> b) -> a -> World -> Pair b World =
  fun @a @b ->
    fun (f : a -> b) (x : a) (world : World) ->
      let y : b = f x in
      seq @b @(Pair b World) y (Pair @b @World y world)
io : ∀a b. (a -> b) -> a -> IO b =
  fun @a @b ->
    fun (f : a -> b) (x : a) ->
      coerce @((World -> Pair b World) -> IO b) (io$ll1 @a @b f x)
print : Int -> IO Unit = fun (n : Int) -> io @Int @Unit puti n
input : IO Int = io @Unit @Int geti Unit
prime : Int =
  add @Int dict$Ring$Int (mul @Int dict$Ring$Int 1000000 1000000) 39
add_mod_prime : Int -> Int -> Int =
  fun (x : Int) (y : Int) ->
    let z : Int = add @Int dict$Ring$Int x y in
    match lt @Int dict$Ord$Int z prime with
    | False -> sub @Int dict$Ring$Int z prime
    | True -> z
fibs0 : List Int = Cons @Int 0 fibs1
fibs1 : List Int =
  Cons @Int 1 (zip_with @Int @Int @Int add_mod_prime fibs0 fibs1)
main$ll1 : Int -> IO Unit =
  fun (n : Int) -> print (nth_exn @Int fibs0 n)
main : IO Unit = bind @IO dict$Monad$IO @Int @Unit input main$ll1
