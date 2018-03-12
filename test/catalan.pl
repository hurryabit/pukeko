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
external mod : Int -> Int -> Int = "mod"
external seq : ∀a b. a -> b -> b = "seq"
external puti : Int -> Unit = "puti"
external geti : Unit -> Int = "geti"
le : ∀a. Ord a -> a -> a -> Bool =
  fun @a (dict : Ord a) ->
    match dict with
    | .Ord _ _ le _ -> le
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
functorList : Functor List = .Functor @List functorList.map.L1
foldableList : Foldable List =
  .Foldable @List foldableList.foldr.L1 foldableList.foldl.L1
bind : ∀m. Monad m -> (∀a b. m a -> (a -> m b) -> m b) =
  fun @m (dict : Monad m) ->
    match dict with
    | .Monad _ bind -> bind
monadIO : Monad IO = .Monad @IO monadIO.pure.L2 monadIO.bind.L2
print : Int -> IO Unit = io.L2 @Int @Unit puti
input : IO Int = io.L2 @Unit @Int geti Unit
p : Int = 100000007
sum_p : List Int -> Int =
  foldl @List foldableList @Int @Int add_p.L1 0
sols : List Int =
  Cons @Int 1 (map @List functorList @(List Int) @Int sols.L1 (scanl.L2 @Int @(List Int) sols.L2 (Nil @Int) sols))
main : IO Unit = bind @IO monadIO @Int @Unit input main.L1
functorList.map.L1 : ∀a b. (a -> b) -> List a -> List b =
  fun @a @b (f : a -> b) (xs : List a) ->
    match xs with
    | Nil -> Nil @b
    | Cons x xs -> Cons @b (f x) (map @List functorList @a @b f xs)
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
mul_p.L1 : Int -> Int -> Int =
  fun (x : Int) (y : Int) -> mod (mul @Int ringInt x y) p
add_p.L1 : Int -> Int -> Int =
  fun (x : Int) (y : Int) -> mod (add @Int ringInt x y) p
scanl.L1 : ∀a b. (b -> a -> b) -> (b -> List a -> List b) -> b -> List a -> List b =
  fun @a @b (f : b -> a -> b) (scanl_f : b -> List a -> List b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> Nil @b
    | Cons x xs ->
      let y0 : b = f y0 x in
      Cons @b y0 (scanl_f y0 xs)
scanl.L2 : ∀a b. (b -> a -> b) -> b -> List a -> List b =
  fun @a @b (f : b -> a -> b) ->
    let rec scanl_f : b -> List a -> List b = scanl.L1 @a @b f scanl_f
    in
    scanl_f
sols.L1 : List Int -> Int =
  fun (xs : List Int) ->
    sum_p (zip_with.L1 @Int @Int @Int mul_p.L1 sols xs)
sols.L2 : List Int -> Int -> List Int =
  fun (xs : List Int) (x : Int) -> Cons @Int x xs
main.L1 : Int -> IO Unit =
  fun (n : Int) -> print (nth_exn.L1 @Int sols n)
