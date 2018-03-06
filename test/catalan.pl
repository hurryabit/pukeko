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
external mod : Int -> Int -> Int = "mod"
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
dict$Functor$List : Functor List =
  let map : ∀a b. (a -> b) -> List a -> List b =
        dict$Functor$List$ll1
  in
  Dict$Functor @List map
dict$Foldable$List : Foldable List =
  let foldr : ∀a b. (a -> b -> b) -> b -> List a -> b =
        dict$Foldable$List$ll1
  and foldl : ∀a b. (b -> a -> b) -> b -> List a -> b =
        dict$Foldable$List$ll2
  in
  Dict$Foldable @List foldr foldl
dict$Monad$IO : Monad IO =
  let pure : ∀a. a -> IO a = dict$Monad$IO$ll2
  and bind : ∀a b. IO a -> (a -> IO b) -> IO b = dict$Monad$IO$ll4
  in
  Dict$Monad @IO pure bind
input : IO Int = io$ll2 @Unit @Int geti Unit
p : Int = 100000007
sum_p : List Int -> Int =
  foldl$ll1 @List dict$Foldable$List @Int @Int add_p$ll1 0
sols : List Int =
  Cons @Int 1 (map$ll1 @List dict$Functor$List @(List Int) @Int sols$ll1 (scanl$ll2 @Int @(List Int) sols$ll2 (Nil @Int) sols))
main : IO Unit =
  bind$ll1 @IO dict$Monad$IO @Int @Unit input main$ll1
le$ll1 : ∀a. Ord a -> a -> a -> Bool =
  fun @a (dict : Ord a) ->
    match dict with
    | Dict$Ord @a _ _ le _ -> le
add$ll1 : ∀a. Ring a -> a -> a -> a =
  fun @a (dict : Ring a) ->
    match dict with
    | Dict$Ring @a _ add _ _ -> add
sub$ll1 : ∀a. Ring a -> a -> a -> a =
  fun @a (dict : Ring a) ->
    match dict with
    | Dict$Ring @a _ _ sub _ -> sub
mul$ll1 : ∀a. Ring a -> a -> a -> a =
  fun @a (dict : Ring a) ->
    match dict with
    | Dict$Ring @a _ _ _ mul -> mul
foldr$ll1 : ∀t. Foldable t -> (∀a b. (a -> b -> b) -> b -> t a -> b) =
  fun @t (dict : Foldable t) ->
    match dict with
    | Dict$Foldable @t foldr _ -> foldr
foldl$ll1 : ∀t. Foldable t -> (∀a b. (b -> a -> b) -> b -> t a -> b) =
  fun @t (dict : Foldable t) ->
    match dict with
    | Dict$Foldable @t _ foldl -> foldl
map$ll1 : ∀f. Functor f -> (∀a b. (a -> b) -> f a -> f b) =
  fun @f (dict : Functor f) ->
    match dict with
    | Dict$Functor @f map -> map
dict$Functor$List$ll1 : ∀a b. (a -> b) -> List a -> List b =
  fun @a @b (f : a -> b) (xs : List a) ->
    match xs with
    | Nil @a -> Nil @b
    | Cons @a x xs ->
      Cons @b (f x) (map$ll1 @List dict$Functor$List @a @b f xs)
dict$Foldable$List$ll1 : ∀a b. (a -> b -> b) -> b -> List a -> b =
  fun @a @b (f : a -> b -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil @a -> y0
    | Cons @a x xs ->
      f x (foldr$ll1 @List dict$Foldable$List @a @b f y0 xs)
dict$Foldable$List$ll2 : ∀a b. (b -> a -> b) -> b -> List a -> b =
  fun @a @b (f : b -> a -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil @a -> y0
    | Cons @a x xs ->
      foldl$ll1 @List dict$Foldable$List @a @b f (f y0 x) xs
nth_exn$ll1 : ∀a. List a -> Int -> a =
  fun @a (xs : List a) (n : Int) ->
    match xs with
    | Nil @a -> abort @a
    | Cons @a x xs ->
      match le$ll1 @Int dict$Ord$Int n 0 with
      | False -> nth_exn$ll1 @a xs (sub$ll1 @Int dict$Ring$Int n 1)
      | True -> x
zip_with$ll1 : ∀a b c. (a -> b -> c) -> List a -> List b -> List c =
  fun @a @b @c (f : a -> b -> c) (xs : List a) (ys : List b) ->
    match xs with
    | Nil @a -> Nil @c
    | Cons @a x xs ->
      match ys with
      | Nil @b -> Nil @c
      | Cons @b y ys -> Cons @c (f x y) (zip_with$ll1 @a @b @c f xs ys)
bind$ll1 : ∀m. Monad m -> (∀a b. m a -> (a -> m b) -> m b) =
  fun @m (dict : Monad m) ->
    match dict with
    | Dict$Monad @m _ bind -> bind
dict$Monad$IO$ll1 : ∀a. a -> World -> Pair a World =
  fun @a -> Pair @a @World
dict$Monad$IO$ll2 : ∀a. a -> IO a =
  fun @a (x : a) -> coerce @(_ -> IO) (dict$Monad$IO$ll1 @a x)
dict$Monad$IO$ll3 : ∀a b. IO a -> (a -> IO b) -> World -> Pair b World =
  fun @a @b (mx : IO a) (f : a -> IO b) (world0 : World) ->
    match coerce @(IO -> _) mx world0 with
    | Pair @a @World x world1 -> coerce @(IO -> _) (f x) world1
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
mul_p$ll1 : Int -> Int -> Int =
  fun (x : Int) (y : Int) -> mod (mul$ll1 @Int dict$Ring$Int x y) p
add_p$ll1 : Int -> Int -> Int =
  fun (x : Int) (y : Int) -> mod (add$ll1 @Int dict$Ring$Int x y) p
scanl$ll1 : ∀a b. (b -> a -> b) -> (b -> List a -> List b) -> b -> List a -> List b =
  fun @a @b (f : b -> a -> b) (scanl_f : b -> List a -> List b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil @a -> Nil @b
    | Cons @a x xs ->
      let y0 : b = f y0 x in
      Cons @b y0 (scanl_f y0 xs)
scanl$ll2 : ∀a b. (b -> a -> b) -> b -> List a -> List b =
  fun @a @b (f : b -> a -> b) ->
    let rec scanl_f : b -> List a -> List b = scanl$ll1 @a @b f scanl_f
    in
    scanl_f
sols$ll1 : List Int -> Int =
  fun (xs : List Int) ->
    sum_p (zip_with$ll1 @Int @Int @Int mul_p$ll1 sols xs)
sols$ll2 : List Int -> Int -> List Int =
  fun (xs : List Int) (x : Int) -> Cons @Int x xs
main$ll1 : Int -> IO Unit =
  fun (n : Int) -> print$ll1 (nth_exn$ll1 @Int sols n)
