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
external abort : ∀a. a = "abort"
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
dict$Monad$IO : Monad IO =
  let pure : ∀a. a -> IO a = fun @a -> dict$Monad$IO$ll2 @a
  and bind : ∀a b. IO a -> (a -> IO b) -> IO b =
        fun @a @b -> dict$Monad$IO$ll4 @a @b
  in
  Dict$Monad @IO pure bind
input : IO Int = io$ll2 @Unit @Int geti Unit
prime : Int =
  add$ll1 @Int dict$Ring$Int (mul$ll1 @Int dict$Ring$Int 1000000 1000000) 39
fibs0 : List Int = Cons @Int 0 fibs1
fibs1 : List Int =
  Cons @Int 1 (zip_with$ll1 @Int @Int @Int add_mod_prime$ll1 fibs0 fibs1)
main : IO Unit =
  bind$ll1 @IO dict$Monad$IO @Int @Unit input main$ll1
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
add$ll1 : ∀a. Ring a -> a -> a -> a =
  fun @a ->
    fun (dict : Ring a) ->
      match dict with
      | Dict$Ring @a _ add _ _ -> add
sub$ll1 : ∀a. Ring a -> a -> a -> a =
  fun @a ->
    fun (dict : Ring a) ->
      match dict with
      | Dict$Ring @a _ _ sub _ -> sub
mul$ll1 : ∀a. Ring a -> a -> a -> a =
  fun @a ->
    fun (dict : Ring a) ->
      match dict with
      | Dict$Ring @a _ _ _ mul -> mul
nth_exn$ll1 : ∀a. List a -> Int -> a =
  fun @a ->
    fun (xs : List a) (n : Int) ->
      match xs with
      | Nil @a -> abort @a
      | Cons @a x xs ->
        match le$ll1 @Int dict$Ord$Int n 0 with
        | False -> nth_exn$ll1 @a xs (sub$ll1 @Int dict$Ring$Int n 1)
        | True -> x
zip_with$ll1 : ∀a b c. (a -> b -> c) -> List a -> List b -> List c =
  fun @a @b @c ->
    fun (f : a -> b -> c) (xs : List a) (ys : List b) ->
      match xs with
      | Nil @a -> Nil @c
      | Cons @a x xs ->
        match ys with
        | Nil @b -> Nil @c
        | Cons @b y ys -> Cons @c (f x y) (zip_with$ll1 @a @b @c f xs ys)
bind$ll1 : ∀m. Monad m -> (∀a b. m a -> (a -> m b) -> m b) =
  fun @m ->
    fun (dict : Monad m) ->
      match dict with
      | Dict$Monad @m _ bind -> bind
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
io$ll1 : ∀a b. (a -> b) -> a -> World -> Pair b World =
  fun @a @b ->
    fun (f : a -> b) (x : a) (world : World) ->
      let y : b = f x in
      seq @b @(Pair b World) y (Pair @b @World y world)
io$ll2 : ∀a b. (a -> b) -> a -> IO b =
  fun @a @b ->
    fun (f : a -> b) (x : a) -> coerce @(_ -> IO) (io$ll1 @a @b f x)
print$ll1 : Int -> IO Unit =
  fun (n : Int) -> io$ll2 @Int @Unit puti n
add_mod_prime$ll1 : Int -> Int -> Int =
  fun (x : Int) (y : Int) ->
    let z : Int = add$ll1 @Int dict$Ring$Int x y in
    match lt$ll1 @Int dict$Ord$Int z prime with
    | False -> sub$ll1 @Int dict$Ring$Int z prime
    | True -> z
main$ll1 : Int -> IO Unit =
  fun (n : Int) -> print$ll1 (nth_exn$ll1 @Int fibs0 n)
