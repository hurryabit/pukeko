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
external le_int : Int -> Int -> Bool = "le"
external add_int : Int -> Int -> Int = "add"
external sub_int : Int -> Int -> Int = "sub"
external mul_int : Int -> Int -> Int = "mul"
external mod : Int -> Int -> Int = "mod"
external seq : ∀a b. a -> b -> b = "seq"
external puti : Int -> Unit = "puti"
external geti : Unit -> Int = "geti"
input : IO Int = coerce @(_ -> IO) (io$ll1 @Unit @Int geti Unit)
p : Int = 100000007
sum_p : List Int -> Int =
  dict$Foldable$List$ll2 @Int @Int add_p$ll1 0
sols : List Int =
  Cons @Int 1 (dict$Functor$List$ll1 @(List Int) @Int sols$ll1 (let rec scanl_f : List Int -> List Int -> List (List Int) =
                                                                          scanl$ll1 @Int @(List Int) sols$ll2 scanl_f
                                                                in
                                                                scanl_f (Nil @Int) sols))
main : IO Unit =
  coerce @(_ -> IO) (dict$Monad$IO$ll3 @Int @Unit input main$ll1)
dict$Functor$List$ll1 : ∀a b. (a -> b) -> List a -> List b =
  fun @a @b (f : a -> b) (xs : List a) ->
    match xs with
    | Nil -> Nil @b
    | Cons x xs -> Cons @b (f x) (dict$Functor$List$ll1 @a @b f xs)
dict$Foldable$List$ll2 : ∀a b. (b -> a -> b) -> b -> List a -> b =
  fun @a @b (f : b -> a -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> y0
    | Cons x xs -> dict$Foldable$List$ll2 @a @b f (f y0 x) xs
nth_exn$ll1 : ∀a. List a -> Int -> a =
  fun @a (xs : List a) (n : Int) ->
    match xs with
    | Nil -> abort @a
    | Cons x xs ->
      match le_int n 0 with
      | False -> nth_exn$ll1 @a xs (sub_int n 1)
      | True -> x
zip_with$ll1 : ∀a b c. (a -> b -> c) -> List a -> List b -> List c =
  fun @a @b @c (f : a -> b -> c) (xs : List a) (ys : List b) ->
    match xs with
    | Nil -> Nil @c
    | Cons x xs ->
      match ys with
      | Nil -> Nil @c
      | Cons y ys -> Cons @c (f x y) (zip_with$ll1 @a @b @c f xs ys)
dict$Monad$IO$ll3 : ∀a b. IO a -> (a -> IO b) -> World -> Pair b World =
  fun @a @b (mx : IO a) (f : a -> IO b) (world0 : World) ->
    match coerce @(IO -> _) mx world0 with
    | Pair x world1 -> coerce @(IO -> _) (f x) world1
io$ll1 : ∀a b. (a -> b) -> a -> World -> Pair b World =
  fun @a @b (f : a -> b) (x : a) (world : World) ->
    let y : b = f x in
    seq @b @(Pair b World) y (Pair @b @World y world)
io$ll2 : ∀a b. (a -> b) -> a -> IO b =
  fun @a @b (f : a -> b) (x : a) ->
    coerce @(_ -> IO) (io$ll1 @a @b f x)
print$ll1 : Int -> IO Unit = io$ll2 @Int @Unit puti
mul_p$ll1 : Int -> Int -> Int =
  fun (x : Int) (y : Int) -> mod (mul_int x y) p
add_p$ll1 : Int -> Int -> Int =
  fun (x : Int) (y : Int) -> mod (add_int x y) p
scanl$ll1 : ∀a b. (b -> a -> b) -> (b -> List a -> List b) -> b -> List a -> List b =
  fun @a @b (f : b -> a -> b) (scanl_f : b -> List a -> List b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> Nil @b
    | Cons x xs ->
      let y0 : b = f y0 x in
      Cons @b y0 (scanl_f y0 xs)
sols$ll1 : List Int -> Int =
  fun (xs : List Int) ->
    sum_p (zip_with$ll1 @Int @Int @Int mul_p$ll1 sols xs)
sols$ll2 : List Int -> Int -> List Int =
  fun (xs : List Int) (x : Int) -> Cons @Int x xs
main$ll1 : Int -> IO Unit =
  fun (n : Int) -> print$ll1 (nth_exn$ll1 @Int sols n)
