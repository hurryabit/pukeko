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
external eq_int : Int -> Int -> Bool = "eq"
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
eq : ∀a. Eq a -> a -> a -> Bool =
  fun @a (dict : Eq a) ->
    match dict with
    | .Eq eq -> eq
le : ∀a. Ord a -> a -> a -> Bool =
  fun @a (dict : Ord a) ->
    match dict with
    | .Ord _ _ le _ -> le
append : ∀m. Monoid m -> m -> m -> m =
  fun @m (dict : Monoid m) ->
    match dict with
    | .Monoid _ append -> append
add : ∀a. Ring a -> a -> a -> a =
  fun @a (dict : Ring a) ->
    match dict with
    | .Ring _ add _ _ -> add
sub : ∀a. Ring a -> a -> a -> a =
  fun @a (dict : Ring a) ->
    match dict with
    | .Ring _ _ sub _ -> sub
eqInt : Eq Int = .Eq @Int eq_int
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
monoidList : ∀a. Monoid (List a) =
  fun @a ->
    .Monoid @(List a) (monoidList.empty @a) (monoidList.append.L1 @a)
foldableList : Foldable List =
  .Foldable @List foldableList.foldr.L1 foldableList.foldl.L1
bind : ∀m. Monad m -> (∀a b. m a -> (a -> m b) -> m b) =
  fun @m (dict : Monad m) ->
    match dict with
    | .Monad _ bind -> bind
monadIO : Monad IO = .Monad @IO monadIO.pure.L2 monadIO.bind.L2
print : Int -> IO Unit = io.L2 @Int @Unit puti
input : IO Int = io.L2 @Unit @Int geti Unit
psums : List Int -> List Int =
  let rec psums0 : ∀_10. Ring _10 -> _10 -> List _10 -> List _10 =
            psums.L1 psums0
  in
  psums0 @Int ringInt 0
sieve : List Int -> List Int =
  fun (ks : List Int) ->
    match ks with
    | Nil -> abort @(List Int)
    | Cons p ks -> Cons @Int p (sieve (filter.L2 @Int (sieve.L1 p) ks))
primes : List Int =
  Cons @Int 2 (Cons @Int 3 (sieve (psums (Cons @Int 5 (repeat.L1 @Int (Cons @Int 2 (Cons @Int 4 (Nil @Int))))))))
main : IO Unit = bind @IO monadIO @Int @Unit input main.L1
monoidList.empty : ∀a. List a = Nil
neq.L1 : ∀a. Eq a -> a -> a -> Bool =
  fun @a (eq.a : Eq a) (x : a) (y : a) ->
    match eq @a eq.a x y with
    | False -> True
    | True -> False
monoidList.append.L1 : ∀a. List a -> List a -> List a =
  fun @a (xs : List a) (ys : List a) ->
    foldr @List foldableList @a @(List a) (Cons @a) ys xs
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
repeat.L1 : ∀a. List a -> List a =
  fun @a (xs : List a) ->
    let rec ys : List a = append @(List a) (monoidList @a) xs ys in
    ys
psums.L1 : (∀_10. Ring _10 -> _10 -> List _10 -> List _10) -> (∀_10. Ring _10 -> _10 -> List _10 -> List _10) =
  fun (psums0 : ∀_10. Ring _10 -> _10 -> List _10 -> List _10) @_10 (ring._10 : Ring _10) (n : _10) (xs : List _10) ->
    match xs with
    | Nil -> Nil @_10
    | Cons x xs ->
      let y : _10 = add @_10 ring._10 x n in
      Cons @_10 y (psums0 @_10 ring._10 y xs)
filter.L1 : ∀a. (a -> Bool) -> (List a -> List a) -> List a -> List a =
  fun @a (p : a -> Bool) (filter_p : List a -> List a) (xs : List a) ->
    match xs with
    | Nil -> Nil @a
    | Cons x xs ->
      let ys : List a = filter_p xs in
      match p x with
      | False -> ys
      | True -> Cons @a x ys
filter.L2 : ∀a. (a -> Bool) -> List a -> List a =
  fun @a (p : a -> Bool) ->
    let rec filter_p : List a -> List a = filter.L1 @a p filter_p in
    filter_p
sieve.L1 : Int -> Int -> Bool =
  fun (p : Int) (k : Int) -> neq.L1 @Int eqInt (mod k p) 0
main.L1 : Int -> IO Unit =
  fun (n : Int) -> print (nth_exn.L1 @Int primes n)
