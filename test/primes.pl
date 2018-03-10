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
dict$Eq$Int : Eq Int = Dict$Eq @Int eq_int
dict$Ord$Int : Ord Int = Dict$Ord @Int ge_int gt_int le_int lt_int
dict$Ring$Int : Ring Int =
  Dict$Ring @Int neg_int add_int sub_int mul_int
dict$Foldable$List : Foldable List =
  Dict$Foldable @List dict$Foldable$List$ll1 dict$Foldable$List$ll2
dict$Monad$IO : Monad IO =
  Dict$Monad @IO dict$Monad$IO$ll2 dict$Monad$IO$ll4
input : IO Int = coerce @(_ -> IO) (io$ll1 @Unit @Int geti Unit)
psums : List Int -> List Int =
  let rec psums0 : ∀_10. Ring _10 -> _10 -> List _10 -> List _10 =
            psums$ll1 psums0
  in
  psums0 @Int dict$Ring$Int 0
primes : List Int =
  Cons @Int 2 (Cons @Int 3 (sieve$ll2 (psums (Cons @Int 5 (let xs : List Int =
                                                                 Cons @Int 2 (Cons @Int 4 (Nil @Int))
                                                           in
                                                           let rec ys : List Int =
                                                                     let dict : Monoid (List Int) =
                                                                           let empty : List Int =
                                                                                 Nil @Int
                                                                           and append : List Int -> List Int -> List Int =
                                                                                 dict$Monoid$List$ll1 @Int
                                                                           in
                                                                           Dict$Monoid @(List Int) empty append
                                                                     in
                                                                     (match dict with
                                                                      | Dict$Monoid _ append ->
                                                                        append) xs ys
                                                           in
                                                           ys)))))
main : IO Unit =
  (match dict$Monad$IO with
   | Dict$Monad _ bind -> bind) @Int @Unit input main$ll1
dict$Monoid$List$ll1 : ∀a. List a -> List a -> List a =
  fun @a (xs : List a) (ys : List a) ->
    (match dict$Foldable$List with
     | Dict$Foldable foldr _ -> foldr) @a @(List a) (Cons @a) ys xs
dict$Foldable$List$ll1 : ∀a b. (a -> b -> b) -> b -> List a -> b =
  fun @a @b (f : a -> b -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> y0
    | Cons x xs ->
      f x ((match dict$Foldable$List with
            | Dict$Foldable foldr _ -> foldr) @a @b f y0 xs)
dict$Foldable$List$ll2 : ∀a b. (b -> a -> b) -> b -> List a -> b =
  fun @a @b (f : b -> a -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> y0
    | Cons x xs ->
      (match dict$Foldable$List with
       | Dict$Foldable _ foldl -> foldl) @a @b f (f y0 x) xs
nth_exn$ll1 : ∀a. List a -> Int -> a =
  fun @a (xs : List a) (n : Int) ->
    match xs with
    | Nil -> abort @a
    | Cons x xs ->
      match (match dict$Ord$Int with
             | Dict$Ord _ _ le _ -> le) n 0 with
      | False ->
        nth_exn$ll1 @a xs ((match dict$Ring$Int with
                            | Dict$Ring _ _ sub _ -> sub) n 1)
      | True -> x
dict$Monad$IO$ll2 : ∀a. a -> IO a =
  fun @a (x : a) -> coerce @(_ -> IO) (Pair @a @World x)
dict$Monad$IO$ll3 : ∀a b. IO a -> (a -> IO b) -> World -> Pair b World =
  fun @a @b (mx : IO a) (f : a -> IO b) (world0 : World) ->
    match coerce @(IO -> _) mx world0 with
    | Pair x world1 -> coerce @(IO -> _) (f x) world1
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
psums$ll1 : (∀_10. Ring _10 -> _10 -> List _10 -> List _10) -> (∀_10. Ring _10 -> _10 -> List _10 -> List _10) =
  fun (psums0 : ∀_10. Ring _10 -> _10 -> List _10 -> List _10) @_10 (dict$Ring$_10 : Ring _10) (n : _10) (xs : List _10) ->
    match xs with
    | Nil -> Nil @_10
    | Cons x xs ->
      let y : _10 =
            (match dict$Ring$_10 with
             | Dict$Ring _ add _ _ -> add) x n
      in
      Cons @_10 y (psums0 @_10 dict$Ring$_10 y xs)
filter$ll1 : ∀a. (a -> Bool) -> (List a -> List a) -> List a -> List a =
  fun @a (p : a -> Bool) (filter_p : List a -> List a) (xs : List a) ->
    match xs with
    | Nil -> Nil @a
    | Cons x xs ->
      let ys : List a = filter_p xs in
      match p x with
      | False -> ys
      | True -> Cons @a x ys
sieve$ll1 : Int -> Int -> Bool =
  fun (p : Int) (k : Int) ->
    let x : Int = mod k p in
    match (match dict$Eq$Int with
           | Dict$Eq eq -> eq) x 0 with
    | False -> True
    | True -> False
sieve$ll2 : List Int -> List Int =
  fun (ks : List Int) ->
    match ks with
    | Nil -> abort @(List Int)
    | Cons p ks ->
      Cons @Int p (sieve$ll2 (let p : Int -> Bool = sieve$ll1 p in
                              let rec filter_p : List Int -> List Int =
                                        filter$ll1 @Int p filter_p
                              in
                              filter_p ks))
main$ll1 : Int -> IO Unit =
  fun (n : Int) -> print$ll1 (nth_exn$ll1 @Int primes n)
