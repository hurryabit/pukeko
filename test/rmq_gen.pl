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
(<) : ∀a. Dict$Ord a -> a -> a -> Bool =
  fun @a ->
    fun (dict : Dict$Ord a) ->
      match dict with
      | Dict$Ord @a (<) (<=) (>=) (>) -> (<)
(<=) : ∀a. Dict$Ord a -> a -> a -> Bool =
  fun @a ->
    fun (dict : Dict$Ord a) ->
      match dict with
      | Dict$Ord @a (<) (<=) (>=) (>) -> (<=)
type Dict$Monoid m =
       | Dict$Monoid m (m -> m -> m)
type Dict$Ring a =
       | Dict$Ring (a -> a) (a -> a -> a) (a -> a -> a) (a -> a -> a)
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
type List a =
       | Nil
       | Cons a (List a)
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
take : ∀a. Int -> List a -> List a =
  fun @a ->
    fun (n : Int) (xs : List a) ->
      match (<=) @Int dict$Ord$Int n 0 with
      | False ->
        match xs with
        | Nil @a -> Nil @a
        | Cons @a x xs ->
          Cons @a x (take @a ((-) @Int dict$Ring$Int n 1) xs)
      | True -> Nil @a
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
pure : ∀m. Dict$Monad m -> (∀a. a -> m a) =
  fun @m ->
    fun (dict : Dict$Monad m) ->
      match dict with
      | Dict$Monad @m pure (>>=) -> pure
(>>=) : ∀m. Dict$Monad m -> (∀a b. m a -> (a -> m b) -> m b) =
  fun @m ->
    fun (dict : Dict$Monad m) ->
      match dict with
      | Dict$Monad @m pure (>>=) -> (>>=)
(;ll1) : ∀a m. m a -> Unit -> m a =
  fun @a @m -> fun (m2 : m a) (x : Unit) -> m2
(;) : ∀a m. Dict$Monad m -> m Unit -> m a -> m a =
  fun @a @m ->
    fun (dict$Monad$m : Dict$Monad m) (m1 : m Unit) (m2 : m a) ->
      (>>=) @m dict$Monad$m @Unit @a m1 ((;ll1) @a @m m2)
sequence$ll1 : ∀a m. Dict$Monad m -> a -> List a -> m (List a) =
  fun @a @m ->
    fun (dict$Monad$m : Dict$Monad m) (x : a) (xs : List a) ->
      pure @m dict$Monad$m @(List a) (Cons @a x xs)
sequence$ll2 : ∀a m. Dict$Monad m -> List (m a) -> a -> m (List a) =
  fun @a @m ->
    fun (dict$Monad$m : Dict$Monad m) (ms : List (m a)) (x : a) ->
      (>>=) @m dict$Monad$m @(List a) @(List a) (sequence @a @m dict$Monad$m ms) (sequence$ll1 @a @m dict$Monad$m x)
sequence : ∀a m. Dict$Monad m -> List (m a) -> m (List a) =
  fun @a @m ->
    fun (dict$Monad$m : Dict$Monad m) (ms : List (m a)) ->
      match ms with
      | Nil @(m a) -> pure @m dict$Monad$m @(List a) (Nil @a)
      | Cons @(m a) m ms ->
        (>>=) @m dict$Monad$m @a @(List a) m (sequence$ll2 @a @m dict$Monad$m ms)
traverse_$ll1 : ∀a m. Dict$Monad m -> (a -> m Unit) -> a -> m Unit -> m Unit =
  fun @a @m ->
    fun (dict$Monad$m : Dict$Monad m) (f : a -> m Unit) (x : a) (m : m Unit) ->
      (;) @Unit @m dict$Monad$m (f x) m
traverse_ : ∀a m t. Dict$Monad m -> Dict$Foldable t -> (a -> m Unit) -> t a -> m Unit =
  fun @a @m @t ->
    fun (dict$Monad$m : Dict$Monad m) (dict$Foldable$t : Dict$Foldable t) (f : a -> m Unit) ->
      foldr @t dict$Foldable$t @a @(m Unit) (traverse_$ll1 @a @m dict$Monad$m f) (pure @m dict$Monad$m @Unit Unit)
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
gen : ∀a. (a -> a) -> a -> List a =
  fun @a -> fun (f : a -> a) (x : a) -> Cons @a x (gen @a f (f x))
split_at : ∀a. Int -> List a -> Pair (List a) (List a) =
  fun @a ->
    fun (n : Int) (xs : List a) ->
      match (<=) @Int dict$Ord$Int n 0 with
      | False ->
        match xs with
        | Nil @a -> Pair @(List a) @(List a) (Nil @a) (Nil @a)
        | Cons @a x xs ->
          match split_at @a ((-) @Int dict$Ring$Int n 1) xs with
          | Pair @(List a) @(List a) ys zs ->
            Pair @(List a) @(List a) (Cons @a x ys) zs
      | True -> Pair @(List a) @(List a) (Nil @a) xs
random$ll1 : Int -> Int =
  fun (x : Int) -> (%) ((*) @Int dict$Ring$Int 91 x) 1000000007
random : List Int = gen @Int random$ll1 1
main$ll1 : Int -> Int -> Int -> IO Unit =
  fun (n : Int) (y : Int) (z : Int) ->
    let y : Int = (%) y n in
    let z : Int = (%) z n in
    match (<) @Int dict$Ord$Int y z with
    | False -> (;) @Unit @IO dict$Monad$IO (print z) (print y)
    | True -> (;) @Unit @IO dict$Monad$IO (print y) (print z)
main$ll2 : List Unit -> IO Unit =
  fun (x : List Unit) -> pure @IO dict$Monad$IO @Unit Unit
main : IO Unit =
  let n : Int = 400000 in
  (;) @Unit @IO dict$Monad$IO (print n) (let m : Int = 100000 in
                                         (;) @Unit @IO dict$Monad$IO (print m) (match split_at @Int n random with
                                                                                | Pair @(List Int) @(List Int) xs random ->
                                                                                  (;) @Unit @IO dict$Monad$IO (traverse_ @Int @IO @List dict$Monad$IO dict$Foldable$List print xs) (match split_at @Int m random with
                                                                                                                                                                                    | Pair @(List Int) @(List Int) ys random ->
                                                                                                                                                                                      let zs : List Int =
                                                                                                                                                                                            take @Int m random
                                                                                                                                                                                      in
                                                                                                                                                                                      (>>=) @IO dict$Monad$IO @(List Unit) @Unit (sequence @Unit @IO dict$Monad$IO (zip_with @Int @Int @(IO Unit) (main$ll1 n) ys zs)) main$ll2)))
