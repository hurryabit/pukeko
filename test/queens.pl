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
external eq_int : Int -> Int -> Bool = "eq"
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
dict$Eq$Int : Eq Int =
  let eq : Int -> Int -> Bool = eq_int in
  Dict$Eq @Int eq
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
dict$Monoid$Int : Monoid Int =
  let empty : Int = 0
  and append : Int -> Int -> Int = add_int
  in
  Dict$Monoid @Int empty append
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
input : IO Int =
  let f : Unit -> Int = geti
  and x : Unit = Unit
  in
  coerce @(_ -> IO) (io$ll1 @Unit @Int f x)
ints : List Int =
  let rec go : Int -> List Int = ints$ll1 go in
  go 1
main : IO Unit =
  let dict : Monad IO = dict$Monad$IO in
  (match dict with
   | Dict$Monad _ bind -> bind) @Int @Unit input main$ll1
foldMap$ll1 : ∀a m. Monoid m -> (a -> m) -> a -> m -> m =
  fun @a @m (dict$Monoid$m : Monoid m) (f : a -> m) (x : a) ->
    let dict : Monoid m = dict$Monoid$m in
    (match dict with
     | Dict$Monoid _ append -> append) (f x)
length$ll1 : ∀a. a -> Int = fun @a (x : a) -> 1
dict$Monoid$List$ll1 : ∀a. List a -> List a -> List a =
  fun @a (xs : List a) (ys : List a) ->
    let dict : Foldable List = dict$Foldable$List in
    (match dict with
     | Dict$Foldable foldr _ -> foldr) @a @(List a) (Cons @a) ys xs
dict$Functor$List$ll1 : ∀a b. (a -> b) -> List a -> List b =
  fun @a @b (f : a -> b) (xs : List a) ->
    match xs with
    | Nil -> Nil @b
    | Cons x xs ->
      Cons @b (f x) (let dict : Functor List = dict$Functor$List in
                     (match dict with
                      | Dict$Functor map -> map) @a @b f xs)
dict$Foldable$List$ll1 : ∀a b. (a -> b -> b) -> b -> List a -> b =
  fun @a @b (f : a -> b -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> y0
    | Cons x xs ->
      f x (let dict : Foldable List = dict$Foldable$List in
           (match dict with
            | Dict$Foldable foldr _ -> foldr) @a @b f y0 xs)
dict$Foldable$List$ll2 : ∀a b. (b -> a -> b) -> b -> List a -> b =
  fun @a @b (f : b -> a -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil -> y0
    | Cons x xs ->
      let dict : Foldable List = dict$Foldable$List in
      (match dict with
       | Dict$Foldable _ foldl -> foldl) @a @b f (f y0 x) xs
take$ll1 : ∀a. Int -> List a -> List a =
  fun @a (n : Int) (xs : List a) ->
    match let dict : Ord Int = dict$Ord$Int in
          (match dict with
           | Dict$Ord _ _ le _ -> le) n 0 with
    | False ->
      match xs with
      | Nil -> Nil @a
      | Cons x xs ->
        Cons @a x (take$ll1 @a (let dict : Ring Int = dict$Ring$Int in
                                (match dict with
                                 | Dict$Ring _ _ sub _ -> sub) n 1) xs)
    | True -> Nil @a
replicate$ll1 : ∀a. Int -> a -> List a =
  fun @a (n : Int) (x : a) ->
    match let dict : Ord Int = dict$Ord$Int in
          (match dict with
           | Dict$Ord _ _ le _ -> le) n 0 with
    | False ->
      Cons @a x (replicate$ll1 @a (let dict : Ring Int = dict$Ring$Int in
                                   (match dict with
                                    | Dict$Ring _ _ sub _ -> sub) n 1) x)
    | True -> Nil @a
zip_with$ll1 : ∀a b c. (a -> b -> c) -> List a -> List b -> List c =
  fun @a @b @c (f : a -> b -> c) (xs : List a) (ys : List b) ->
    match xs with
    | Nil -> Nil @c
    | Cons x xs ->
      match ys with
      | Nil -> Nil @c
      | Cons y ys -> Cons @c (f x y) (zip_with$ll1 @a @b @c f xs ys)
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
diff$ll1 : List Int -> List Int -> List Int =
  fun (xs : List Int) (ys : List Int) ->
    match xs with
    | Nil -> Nil @Int
    | Cons x xs' ->
      match ys with
      | Nil -> xs
      | Cons y ys' ->
        match let dict : Ord Int = dict$Ord$Int in
              (match dict with
               | Dict$Ord _ _ _ lt -> lt) x y with
        | False ->
          match let dict : Eq Int = dict$Eq$Int in
                (match dict with
                 | Dict$Eq eq -> eq) x y with
          | False -> diff$ll1 xs ys'
          | True -> diff$ll1 xs' ys'
        | True -> Cons @Int x (diff$ll1 xs' ys)
ints$ll1 : (Int -> List Int) -> Int -> List Int =
  fun (go : Int -> List Int) (k : Int) ->
    Cons @Int k (go (let dict : Ring Int = dict$Ring$Int in
                     (match dict with
                      | Dict$Ring _ add _ _ -> add) k 1))
solve_aux$ll1 : Int -> List Int -> Int -> List Int =
  fun (k : Int) (ls : List Int) (i : Int) ->
    diff$ll1 ls (Cons @Int (let dict : Ring Int = dict$Ring$Int in
                            (match dict with
                             | Dict$Ring _ _ sub _ ->
                               sub) k i) (Cons @Int k (Cons @Int (let dict : Ring Int =
                                                                        dict$Ring$Int
                                                                  in
                                                                  (match dict with
                                                                   | Dict$Ring _ add _ _ ->
                                                                     add) k i) (Nil @Int))))
solve_aux$ll2 : List (List Int) -> Int -> List (List Int) =
  fun (kss : List (List Int)) (k : Int) ->
    let dict : Functor List = dict$Functor$List in
    (match dict with
     | Dict$Functor map ->
       map) @(List Int) @(List Int) (Cons @Int k) (solve_aux$ll3 (zip_with$ll1 @(List Int) @Int @(List Int) (solve_aux$ll1 k) kss ints))
solve_aux$ll3 : List (List Int) -> List (List Int) =
  fun (kss : List (List Int)) ->
    match kss with
    | Nil -> Cons @(List Int) (Nil @Int) (Nil @(List Int))
    | Cons ks kss ->
      let dict$Foldable$t : Foldable List = dict$Foldable$List
      and dict$Monoid$m : Monoid (List (List Int)) =
            let empty : List (List Int) = Nil @(List Int)
            and append : List (List Int) -> List (List Int) -> List (List Int) =
                  dict$Monoid$List$ll1 @(List Int)
            in
            Dict$Monoid @(List (List Int)) empty append
      and f : Int -> List (List Int) = solve_aux$ll2 kss
      in
      let dict : Foldable List = dict$Foldable$t in
      (match dict with
       | Dict$Foldable foldr _ ->
         foldr) @Int @(List (List Int)) (foldMap$ll1 @Int @(List (List Int)) dict$Monoid$m f) (let dict : Monoid (List (List Int)) =
                                                                                                     dict$Monoid$m
                                                                                               in
                                                                                               match dict with
                                                                                               | Dict$Monoid empty _ ->
                                                                                                 empty) ks
main$ll1 : Int -> IO Unit =
  fun (n : Int) ->
    print$ll1 (let dict$Foldable$t : Foldable List = dict$Foldable$List
               in
               let dict$Foldable$t : Foldable List = dict$Foldable$t
               and dict$Monoid$m : Monoid Int = dict$Monoid$Int
               and f : List Int -> Int = length$ll1 @(List Int)
               in
               let dict : Foldable List = dict$Foldable$t in
               (match dict with
                | Dict$Foldable foldr _ ->
                  foldr) @(List Int) @Int (foldMap$ll1 @(List Int) @Int dict$Monoid$m f) (let dict : Monoid Int =
                                                                                                dict$Monoid$m
                                                                                          in
                                                                                          match dict with
                                                                                          | Dict$Monoid empty _ ->
                                                                                            empty) (let n : Int =
                                                                                                          n
                                                                                                    in
                                                                                                    solve_aux$ll3 (replicate$ll1 @(List Int) n (take$ll1 @Int n ints))))
