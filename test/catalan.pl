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
ordInt : Ord Int = .Ord @Int ge_int gt_int le_int lt_int
ringInt : Ring Int = .Ring @Int neg_int add_int sub_int mul_int
functorList : Functor List = .Functor @List functorList.map.L1
foldableList : Foldable List =
  .Foldable @List foldableList.foldr.L1 foldableList.foldl.L1
monadIO : Monad IO = .Monad @IO monadIO.pure.L2 monadIO.bind.L2
print : Int -> IO Unit = io.L2 @Int @Unit puti
input : IO Int =
  let f : Unit -> Int = geti
  and x : Unit = Unit
  in
  coerce @(_ -> IO) (io.L1 @Unit @Int f x)
p : Int = 100000007
sum_p : List Int -> Int =
  let dict : Foldable List = foldableList in
  (match dict with
   | .Foldable _ foldl -> foldl) @Int @Int add_p.L1 0
sols : List Int =
  Cons @Int 1 (let dict : Functor List = functorList in
               (match dict with
                | .Functor map ->
                  map) @(List Int) @Int sols.L1 (let f : List Int -> Int -> List Int =
                                                       sols.L2
                                                 in
                                                 let rec scanl_f : List Int -> List Int -> List (List Int) =
                                                           scanl.L1 @Int @(List Int) f scanl_f
                                                 in
                                                 scanl_f (Nil @Int) sols))
main : IO Unit =
  let dict : Monad IO = monadIO in
  (match dict with
   | .Monad _ bind -> bind) @Int @Unit input main.L1
functorList.map.L1 : ∀a b. (a -> b) -> List a -> List b =
  fun @a @b (f : a -> b) (xs : List a) ->
    match xs with
    | Nil -> Nil @b
    | Cons x xs ->
      Cons @b (f x) (let dict : Functor List = functorList in
                     (match dict with
                      | .Functor map -> map) @a @b f xs)
foldableList.foldr.L1 : ∀a b. (a -> b -> b) -> b -> List a -> b =
  fun @a @b (f : a -> b -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> y0
    | Cons x xs ->
      f x (let dict : Foldable List = foldableList in
           (match dict with
            | .Foldable foldr _ -> foldr) @a @b f y0 xs)
foldableList.foldl.L1 : ∀a b. (b -> a -> b) -> b -> List a -> b =
  fun @a @b (f : b -> a -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> y0
    | Cons x xs ->
      let dict : Foldable List = foldableList in
      (match dict with
       | .Foldable _ foldl -> foldl) @a @b f (f y0 x) xs
nth_exn.L1 : ∀a. List a -> Int -> a =
  fun @a (xs : List a) (n : Int) ->
    match xs with
    | Nil -> abort @a
    | Cons x xs ->
      match let dict : Ord Int = ordInt in
            (match dict with
             | .Ord _ _ le _ -> le) n 0 with
      | False ->
        nth_exn.L1 @a xs (let dict : Ring Int = ringInt in
                          (match dict with
                           | .Ring _ _ sub _ -> sub) n 1)
      | True -> x
zip_with.L1 : ∀a b c. (a -> b -> c) -> List a -> List b -> List c =
  fun @a @b @c (f : a -> b -> c) (xs : List a) (ys : List b) ->
    match xs with
    | Nil -> Nil @c
    | Cons x xs ->
      match ys with
      | Nil -> Nil @c
      | Cons y ys -> Cons @c (f x y) (zip_with.L1 @a @b @c f xs ys)
monadIO.pure.L2 : ∀a. a -> IO a =
  fun @a (x : a) -> coerce @(_ -> IO) (Pair @a @World x)
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
  fun (x : Int) (y : Int) ->
    mod (let dict : Ring Int = ringInt in
         (match dict with
          | .Ring _ _ _ mul -> mul) x y) p
add_p.L1 : Int -> Int -> Int =
  fun (x : Int) (y : Int) ->
    mod (let dict : Ring Int = ringInt in
         (match dict with
          | .Ring _ add _ _ -> add) x y) p
scanl.L1 : ∀a b. (b -> a -> b) -> (b -> List a -> List b) -> b -> List a -> List b =
  fun @a @b (f : b -> a -> b) (scanl_f : b -> List a -> List b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> Nil @b
    | Cons x xs ->
      let y0 : b = f y0 x in
      Cons @b y0 (scanl_f y0 xs)
sols.L1 : List Int -> Int =
  fun (xs : List Int) ->
    sum_p (zip_with.L1 @Int @Int @Int mul_p.L1 sols xs)
sols.L2 : List Int -> Int -> List Int =
  fun (xs : List Int) (x : Int) -> Cons @Int x xs
main.L1 : Int -> IO Unit =
  fun (n : Int) -> print (nth_exn.L1 @Int sols n)
