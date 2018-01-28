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
external (%) : Int -> Int -> Int = "mod"
external (<) : Int -> Int -> Bool = "lt"
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
take : ∀a. Int -> List a -> List a =
  fun @a ->
    fun (n : Int) (xs : List a) ->
      match (<=) n 0 with
      | False ->
        match xs with
        | Nil @a -> Nil @a
        | Cons @a x xs -> Cons @a x (take @a ((-) n 1) xs)
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
type IO a
external return : ∀a. a -> IO a = "return"
external print : Int -> IO Unit = "print"
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
gen : ∀a. (a -> a) -> a -> List a =
  fun @a -> fun (f : a -> a) (x : a) -> Cons @a x (gen @a f (f x))
split_at : ∀a. Int -> List a -> Pair (List a) (List a) =
  fun @a ->
    fun (n : Int) (xs : List a) ->
      match (<=) n 0 with
      | False ->
        match xs with
        | Nil @a -> Pair @(List a) @(List a) (Nil @a) (Nil @a)
        | Cons @a x xs ->
          match split_at @a ((-) n 1) xs with
          | Pair @(List a) @(List a) ys zs ->
            Pair @(List a) @(List a) (Cons @a x ys) zs
      | True -> Pair @(List a) @(List a) (Nil @a) xs
random$ll1 : Int -> Int =
  fun (x : Int) -> (%) ((*) 91 x) 1000000007
random : List Int = gen @Int random$ll1 1
main$ll1 : Int -> Int -> Int -> IO Unit =
  fun (n : Int) (y : Int) (z : Int) ->
    let y : Int = (%) y n in
    let z : Int = (%) z n in
    match (<) y z with
    | False -> (;) @Unit (print z) (print y)
    | True -> (;) @Unit (print y) (print z)
main$ll2 : List Unit -> IO Unit =
  fun (x : List Unit) -> return @Unit Unit
main : IO Unit =
  let n : Int = 400000 in
  (;) @Unit (print n) (let m : Int = 100000 in
                       (;) @Unit (print m) (match split_at @Int n random with
                                            | Pair @(List Int) @(List Int) xs random ->
                                              (;) @Unit (iter_io @Int print xs) (match split_at @Int m random with
                                                                                 | Pair @(List Int) @(List Int) ys random ->
                                                                                   let zs : List Int =
                                                                                         take @Int m random
                                                                                   in
                                                                                   (>>=) @(List Unit) @Unit (sequence_io @Unit (zip_with @Int @Int @(IO Unit) (main$ll1 n) ys zs)) main$ll2)))
