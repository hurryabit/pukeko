external abort : ∀a. a = "abort"
type Unit =
       | Unit
type Bool =
       | False
       | True
type Pair a b =
       | Pair a b
type Option a =
       | None
       | Some a
type Choice a b =
       | First a
       | Second b
type Dict$Eq a =
       | Dict$Eq (a -> a -> Bool)
type Dict$Ord a =
       | Dict$Ord (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool)
(<=) : ∀a. Dict$Ord a -> a -> a -> Bool =
  fun @a ->
    fun (dict : Dict$Ord a) ->
      match dict with
      | Dict$Ord @a (<) (<=) (>=) (>) -> (<=)
type Dict$Monoid m =
       | Dict$Monoid m (m -> m -> m)
type Dict$Ring a =
       | Dict$Ring (a -> a) (a -> a -> a) (a -> a -> a) (a -> a -> a)
(+) : ∀a. Dict$Ring a -> a -> a -> a =
  fun @a ->
    fun (dict : Dict$Ring a) ->
      match dict with
      | Dict$Ring @a neg (+) (-) (*) -> (+)
(-) : ∀a. Dict$Ring a -> a -> a -> a =
  fun @a ->
    fun (dict : Dict$Ring a) ->
      match dict with
      | Dict$Ring @a neg (+) (-) (*) -> (-)
(*) : ∀a. Dict$Ring a -> a -> a -> a =
  fun @a ->
    fun (dict : Dict$Ring a) ->
      match dict with
      | Dict$Ring @a neg (+) (-) (*) -> (*)
type Int
external lt_int : Int -> Int -> Bool = "lt"
external le_int : Int -> Int -> Bool = "le"
external ge_int : Int -> Int -> Bool = "ge"
external gt_int : Int -> Int -> Bool = "gt"
dict$Ord$Int : Dict$Ord Int =
  let (<) : Int -> Int -> Bool = lt_int
  and (<=) : Int -> Int -> Bool = le_int
  and (>=) : Int -> Int -> Bool = ge_int
  and (>) : Int -> Int -> Bool = gt_int
  in
  Dict$Ord @Int (<) (<=) (>=) (>)
external neg_int : Int -> Int = "neg"
external add_int : Int -> Int -> Int = "add"
external sub_int : Int -> Int -> Int = "sub"
external mul_int : Int -> Int -> Int = "mul"
dict$Ring$Int : Dict$Ring Int =
  let neg : Int -> Int = neg_int
  and (+) : Int -> Int -> Int = add_int
  and (-) : Int -> Int -> Int = sub_int
  and (*) : Int -> Int -> Int = mul_int
  in
  Dict$Ring @Int neg (+) (-) (*)
external (%) : Int -> Int -> Int = "mod"
type Char
type Dict$Foldable t =
       | Dict$Foldable (∀a b. (a -> b -> b) -> b -> t a -> b) (∀a b. (b -> a -> b) -> b -> t a -> b)
foldr : ∀t. Dict$Foldable t -> (∀a b. (a -> b -> b) -> b -> t a -> b) =
  fun @t ->
    fun (dict : Dict$Foldable t) ->
      match dict with
      | Dict$Foldable @t foldr foldl -> foldr
foldl : ∀t. Dict$Foldable t -> (∀a b. (b -> a -> b) -> b -> t a -> b) =
  fun @t ->
    fun (dict : Dict$Foldable t) ->
      match dict with
      | Dict$Foldable @t foldr foldl -> foldl
type Dict$Functor f =
       | Dict$Functor (∀a b. (a -> b) -> f a -> f b)
map : ∀f. Dict$Functor f -> (∀a b. (a -> b) -> f a -> f b) =
  fun @f ->
    fun (dict : Dict$Functor f) ->
      match dict with
      | Dict$Functor @f map -> map
type List a =
       | Nil
       | Cons a (List a)
dict$Functor$List$ll1 : ∀a b. (a -> b) -> List a -> List b =
  fun @a @b ->
    fun (f : a -> b) (xs : List a) ->
      match xs with
      | Nil @a -> Nil @b
      | Cons @a x xs ->
        Cons @b (f x) (map @List dict$Functor$List @a @b f xs)
dict$Functor$List : Dict$Functor List =
  let map : ∀a b. (a -> b) -> List a -> List b =
        fun @a @b -> dict$Functor$List$ll1 @a @b
  in
  Dict$Functor @List map
dict$Foldable$List$ll1 : ∀a b. (a -> b -> b) -> b -> List a -> b =
  fun @a @b ->
    fun (f : a -> b -> b) (y0 : b) (xs : List a) ->
      match xs with
      | Nil @a -> y0
      | Cons @a x xs ->
        f x (foldr @List dict$Foldable$List @a @b f y0 xs)
dict$Foldable$List$ll2 : ∀a b. (b -> a -> b) -> b -> List a -> b =
  fun @a @b ->
    fun (f : b -> a -> b) (y0 : b) (xs : List a) ->
      match xs with
      | Nil @a -> y0
      | Cons @a x xs ->
        foldl @List dict$Foldable$List @a @b f (f y0 x) xs
dict$Foldable$List : Dict$Foldable List =
  let foldr : ∀a b. (a -> b -> b) -> b -> List a -> b =
        fun @a @b -> dict$Foldable$List$ll1 @a @b
  and foldl : ∀a b. (b -> a -> b) -> b -> List a -> b =
        fun @a @b -> dict$Foldable$List$ll2 @a @b
  in
  Dict$Foldable @List foldr foldl
nth_exn : ∀a. List a -> Int -> a =
  fun @a ->
    fun (xs : List a) (n : Int) ->
      match xs with
      | Nil @a -> abort @a
      | Cons @a x xs ->
        match (<=) @Int dict$Ord$Int n 0 with
        | False -> nth_exn @a xs ((-) @Int dict$Ring$Int n 1)
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
type Dict$Monad m =
       | Dict$Monad (∀a. a -> m a) (∀a b. m a -> (a -> m b) -> m b)
(>>=) : ∀m. Dict$Monad m -> (∀a b. m a -> (a -> m b) -> m b) =
  fun @m ->
    fun (dict : Dict$Monad m) ->
      match dict with
      | Dict$Monad @m pure (>>=) -> (>>=)
type IO a
external pure_io : ∀a. a -> IO a = "return"
external bind_io : ∀a b. IO a -> (a -> IO b) -> IO b = "bind"
dict$Monad$IO : Dict$Monad IO =
  let pure : ∀a. a -> IO a = fun @a -> pure_io @a
  and (>>=) : ∀a b. IO a -> (a -> IO b) -> IO b =
        fun @a @b -> bind_io @a @b
  in
  Dict$Monad @IO pure (>>=)
external print : Int -> IO Unit = "print"
external input : IO Int = "input"
p : Int = 100000007
mul_p : Int -> Int -> Int =
  fun (x : Int) (y : Int) -> (%) ((*) @Int dict$Ring$Int x y) p
add_p : Int -> Int -> Int =
  fun (x : Int) (y : Int) -> (%) ((+) @Int dict$Ring$Int x y) p
sum_p : List Int -> Int =
  foldl @List dict$Foldable$List @Int @Int add_p 0
scanl$ll1 : ∀a b. (b -> a -> b) -> (b -> List a -> List b) -> b -> List a -> List b =
  fun @a @b ->
    fun (f : b -> a -> b) (scanl_f : b -> List a -> List b) (y0 : b) (xs : List a) ->
      match xs with
      | Nil @a -> Nil @b
      | Cons @a x xs ->
        let y0 : b = f y0 x in
        Cons @b y0 (scanl_f y0 xs)
scanl : ∀a b. (b -> a -> b) -> b -> List a -> List b =
  fun @a @b ->
    fun (f : b -> a -> b) ->
      let rec scanl_f : b -> List a -> List b = scanl$ll1 @a @b f scanl_f
      in
      scanl_f
sols$ll1 : List Int -> Int =
  fun (xs : List Int) ->
    sum_p (zip_with @Int @Int @Int mul_p sols xs)
sols$ll2 : List Int -> Int -> List Int =
  fun (xs : List Int) (x : Int) -> Cons @Int x xs
sols : List Int =
  Cons @Int 1 (map @List dict$Functor$List @(List Int) @Int sols$ll1 (scanl @Int @(List Int) sols$ll2 (Nil @Int) sols))
main$ll1 : Int -> IO Unit =
  fun (n : Int) -> print (nth_exn @Int sols n)
main : IO Unit = (>>=) @IO dict$Monad$IO @Int @Unit input main$ll1
