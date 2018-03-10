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
data BinTree a =
       | Leaf
       | Branch (BinTree a) a (BinTree a)
data Bag a = BinTree a
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
dict$Foldable$BinTree : Foldable BinTree =
  let foldr : ∀a b. (a -> b -> b) -> b -> BinTree a -> b =
        dict$Foldable$BinTree$ll1
  and foldl : ∀a b. (b -> a -> b) -> b -> BinTree a -> b =
        dict$Foldable$BinTree$ll2
  in
  Dict$Foldable @BinTree foldr foldl
dict$Foldable$Bag : Foldable Bag =
  let foldr : ∀a b. (a -> b -> b) -> b -> Bag a -> b =
        dict$Foldable$Bag$ll1
  and foldl : ∀a b. (b -> a -> b) -> b -> Bag a -> b =
        dict$Foldable$Bag$ll2
  in
  Dict$Foldable @Bag foldr foldl
main : IO Unit =
  let dict : Monad IO = dict$Monad$IO in
  (match dict with
   | Dict$Monad _ bind -> bind) @Int @Unit input main$ll2
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
semi$ll1 : ∀a m. m a -> Unit -> m a =
  fun @a @m (m2 : m a) (x : Unit) -> m2
semi$ll2 : ∀a m. Monad m -> m Unit -> m a -> m a =
  fun @a @m (dict$Monad$m : Monad m) (m1 : m Unit) (m2 : m a) ->
    let dict : Monad m = dict$Monad$m in
    (match dict with
     | Dict$Monad _ bind -> bind) @Unit @a m1 (semi$ll1 @a @m m2)
sequence$ll1 : ∀a m. Monad m -> a -> List a -> m (List a) =
  fun @a @m (dict$Monad$m : Monad m) (x : a) (xs : List a) ->
    let dict : Monad m = dict$Monad$m in
    (match dict with
     | Dict$Monad pure _ -> pure) @(List a) (Cons @a x xs)
sequence$ll2 : ∀a m. Monad m -> List (m a) -> a -> m (List a) =
  fun @a @m (dict$Monad$m : Monad m) (ms : List (m a)) (x : a) ->
    let dict : Monad m = dict$Monad$m in
    (match dict with
     | Dict$Monad _ bind ->
       bind) @(List a) @(List a) (sequence$ll3 @a @m dict$Monad$m ms) (sequence$ll1 @a @m dict$Monad$m x)
sequence$ll3 : ∀a m. Monad m -> List (m a) -> m (List a) =
  fun @a @m (dict$Monad$m : Monad m) (ms : List (m a)) ->
    match ms with
    | Nil ->
      let dict : Monad m = dict$Monad$m in
      (match dict with
       | Dict$Monad pure _ -> pure) @(List a) (Nil @a)
    | Cons m ms ->
      let dict : Monad m = dict$Monad$m in
      (match dict with
       | Dict$Monad _ bind ->
         bind) @a @(List a) m (sequence$ll2 @a @m dict$Monad$m ms)
traverse_$ll1 : ∀a m. Monad m -> (a -> m Unit) -> a -> m Unit -> m Unit =
  fun @a @m (dict$Monad$m : Monad m) (f : a -> m Unit) (x : a) ->
    semi$ll2 @Unit @m dict$Monad$m (f x)
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
dict$Foldable$BinTree$ll1 : ∀a b. (a -> b -> b) -> b -> BinTree a -> b =
  fun @a @b (f : a -> b -> b) (y0 : b) (t : BinTree a) ->
    match t with
    | Leaf -> y0
    | Branch l x r ->
      let dict : Foldable BinTree = dict$Foldable$BinTree in
      (match dict with
       | Dict$Foldable foldr _ ->
         foldr) @a @b f (f x (let dict : Foldable BinTree =
                                    dict$Foldable$BinTree
                              in
                              (match dict with
                               | Dict$Foldable foldr _ -> foldr) @a @b f y0 r)) l
dict$Foldable$BinTree$ll2 : ∀a b. (b -> a -> b) -> b -> BinTree a -> b =
  fun @a @b (f : b -> a -> b) (y0 : b) (t : BinTree a) ->
    match t with
    | Leaf -> y0
    | Branch l x r ->
      let dict : Foldable BinTree = dict$Foldable$BinTree in
      (match dict with
       | Dict$Foldable _ foldl ->
         foldl) @a @b f (f (let dict : Foldable BinTree =
                                  dict$Foldable$BinTree
                            in
                            (match dict with
                             | Dict$Foldable _ foldl -> foldl) @a @b f y0 l) x) r
dict$Foldable$Bag$ll1 : ∀a b. (a -> b -> b) -> b -> Bag a -> b =
  fun @a @b (f : a -> b -> b) (y0 : b) (bag : Bag a) ->
    let dict : Foldable BinTree = dict$Foldable$BinTree in
    (match dict with
     | Dict$Foldable foldr _ ->
       foldr) @a @b f y0 (coerce @(Bag -> _) bag)
dict$Foldable$Bag$ll2 : ∀a b. (b -> a -> b) -> b -> Bag a -> b =
  fun @a @b (f : b -> a -> b) (y0 : b) (bag : Bag a) ->
    let dict : Foldable BinTree = dict$Foldable$BinTree in
    (match dict with
     | Dict$Foldable _ foldl ->
       foldl) @a @b f y0 (coerce @(Bag -> _) bag)
bag_insert$ll1 : (∀_14. Ord _14 -> _14 -> BinTree _14 -> BinTree _14) -> (∀_14. Ord _14 -> _14 -> BinTree _14 -> BinTree _14) =
  fun (insert : ∀_14. Ord _14 -> _14 -> BinTree _14 -> BinTree _14) @_14 (dict$Ord$_14 : Ord _14) (x : _14) (t : BinTree _14) ->
    match t with
    | Leaf -> Branch @_14 (Leaf @_14) x (Leaf @_14)
    | Branch l y r ->
      match let dict : Ord _14 = dict$Ord$_14 in
            (match dict with
             | Dict$Ord _ _ _ lt -> lt) x y with
      | False -> Branch @_14 l y (insert @_14 dict$Ord$_14 x r)
      | True -> Branch @_14 (insert @_14 dict$Ord$_14 x l) y r
tsort$ll1 : Bag Int -> Int -> Bag Int =
  fun (s : Bag Int) (x : Int) ->
    let dict$Ord$a : Ord Int = dict$Ord$Int
    and x : Int = x
    and s : Bag Int = s
    in
    let rec insert : ∀_14. Ord _14 -> _14 -> BinTree _14 -> BinTree _14 =
              bag_insert$ll1 insert
    in
    coerce @(_ -> Bag) (insert @Int dict$Ord$a x (coerce @(Bag -> _) s))
main$ll1 : List Int -> IO Unit =
  fun (xs : List Int) ->
    let dict$Monad$m : Monad IO = dict$Monad$IO
    and dict$Foldable$t : Foldable List = dict$Foldable$List
    and f : Int -> IO Unit = print$ll1
    in
    let dict : Foldable List = dict$Foldable$t in
    (match dict with
     | Dict$Foldable foldr _ ->
       foldr) @Int @(IO Unit) (traverse_$ll1 @Int @IO dict$Monad$m f) (let dict : Monad IO =
                                                                             dict$Monad$m
                                                                       in
                                                                       (match dict with
                                                                        | Dict$Monad pure _ ->
                                                                          pure) @Unit Unit) (let xs : List Int =
                                                                                                   xs
                                                                                             in
                                                                                             let dict$Foldable$t : Foldable Bag =
                                                                                                   dict$Foldable$Bag
                                                                                             in
                                                                                             let dict : Foldable Bag =
                                                                                                   dict$Foldable$t
                                                                                             in
                                                                                             (match dict with
                                                                                              | Dict$Foldable foldr _ ->
                                                                                                foldr) @Int @(List Int) (Cons @Int) (Nil @Int) (let dict : Foldable List =
                                                                                                                                                      dict$Foldable$List
                                                                                                                                                in
                                                                                                                                                (match dict with
                                                                                                                                                 | Dict$Foldable _ foldl ->
                                                                                                                                                   foldl) @Int @(Bag Int) tsort$ll1 (coerce @(_ -> Bag) (Leaf @Int)) xs))
main$ll2 : Int -> IO Unit =
  fun (n : Int) ->
    let dict : Monad IO = dict$Monad$IO in
    (match dict with
     | Dict$Monad _ bind ->
       bind) @(List Int) @Unit (sequence$ll3 @Int @IO dict$Monad$IO (replicate$ll1 @(IO Int) n input)) main$ll1
