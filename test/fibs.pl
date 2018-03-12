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
le : ∀a. Ord a -> a -> a -> Bool =
  fun @a (dict : Ord a) ->
    match dict with
    | .Ord _ _ le _ -> le
lt : ∀a. Ord a -> a -> a -> Bool =
  fun @a (dict : Ord a) ->
    match dict with
    | .Ord _ _ _ lt -> lt
add : ∀a. Ring a -> a -> a -> a =
  fun @a (dict : Ring a) ->
    match dict with
    | .Ring _ add _ _ -> add
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
bind : ∀m. Monad m -> (∀a b. m a -> (a -> m b) -> m b) =
  fun @m (dict : Monad m) ->
    match dict with
    | .Monad _ bind -> bind
monadIO : Monad IO = .Monad @IO monadIO.pure.L2 monadIO.bind.L2
print : Int -> IO Unit = io.L2 @Int @Unit puti
input : IO Int = io.L2 @Unit @Int geti Unit
prime : Int =
  add @Int ringInt (mul @Int ringInt 1000000 1000000) 39
fibs0 : List Int = Cons @Int 0 fibs1
fibs1 : List Int =
  Cons @Int 1 (zip_with.L1 @Int @Int @Int add_mod_prime.L1 fibs0 fibs1)
main : IO Unit = bind @IO monadIO @Int @Unit input main.L1
nth_exn.L1 : ∀a. List a -> Int -> a =
  fun @a (xs : List a) (n : Int) ->
    match xs with
    | Nil -> abort @a
    | Cons x xs ->
      match le @Int ordInt n 0 with
      | False -> nth_exn.L1 @a xs (sub @Int ringInt n 1)
      | True -> x
zip_with.L1 : ∀a b c. (a -> b -> c) -> List a -> List b -> List c =
  fun @a @b @c (f : a -> b -> c) (xs : List a) (ys : List b) ->
    match xs with
    | Nil -> Nil @c
    | Cons x xs ->
      match ys with
      | Nil -> Nil @c
      | Cons y ys -> Cons @c (f x y) (zip_with.L1 @a @b @c f xs ys)
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
add_mod_prime.L1 : Int -> Int -> Int =
  fun (x : Int) (y : Int) ->
    let z : Int = add @Int ringInt x y in
    match lt @Int ordInt z prime with
    | False -> sub @Int ringInt z prime
    | True -> z
main.L1 : Int -> IO Unit =
  fun (n : Int) -> print (nth_exn.L1 @Int fibs0 n)
