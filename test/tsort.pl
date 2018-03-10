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
ordInt : Ord Int = .Ord @Int ge_int gt_int le_int lt_int
ringInt : Ring Int = .Ring @Int neg_int add_int sub_int mul_int
foldableList : Foldable List =
  .Foldable @List foldableList.foldr.L1 foldableList.foldl.L1
monadIO : Monad IO = .Monad @IO monadIO.pure.L2 monadIO.bind.L2
print : Int -> IO Unit = io.L2 @Int @Unit puti
input : IO Int =
  let f : Unit -> Int = geti
  and x : Unit = Unit
  in
  coerce @(_ -> IO) (io.L1 @Unit @Int f x)
foldableBinTree : Foldable BinTree =
  .Foldable @BinTree foldableBinTree.foldr.L1 foldableBinTree.foldl.L1
foldableBag : Foldable Bag =
  .Foldable @Bag foldableBag.foldr.L1 foldableBag.foldl.L1
main : IO Unit =
  let dict : Monad IO = monadIO in
  (match dict with
   | .Monad _ bind -> bind) @Int @Unit input main.L2
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
replicate.L1 : ∀a. Int -> a -> List a =
  fun @a (n : Int) (x : a) ->
    match let dict : Ord Int = ordInt in
          (match dict with
           | .Ord _ _ le _ -> le) n 0 with
    | False ->
      Cons @a x (replicate.L1 @a (let dict : Ring Int = ringInt in
                                  (match dict with
                                   | .Ring _ _ sub _ -> sub) n 1) x)
    | True -> Nil @a
semi.L1 : ∀a m. m a -> Unit -> m a =
  fun @a @m (m2 : m a) (x : Unit) -> m2
semi.L2 : ∀a m. Monad m -> m Unit -> m a -> m a =
  fun @a @m (monad.m : Monad m) (m1 : m Unit) (m2 : m a) ->
    let dict : Monad m = monad.m in
    (match dict with
     | .Monad _ bind -> bind) @Unit @a m1 (semi.L1 @a @m m2)
sequence.L1 : ∀a m. Monad m -> a -> List a -> m (List a) =
  fun @a @m (monad.m : Monad m) (x : a) (xs : List a) ->
    let dict : Monad m = monad.m in
    (match dict with
     | .Monad pure _ -> pure) @(List a) (Cons @a x xs)
sequence.L2 : ∀a m. Monad m -> List (m a) -> a -> m (List a) =
  fun @a @m (monad.m : Monad m) (ms : List (m a)) (x : a) ->
    let dict : Monad m = monad.m in
    (match dict with
     | .Monad _ bind ->
       bind) @(List a) @(List a) (sequence.L3 @a @m monad.m ms) (sequence.L1 @a @m monad.m x)
sequence.L3 : ∀a m. Monad m -> List (m a) -> m (List a) =
  fun @a @m (monad.m : Monad m) (ms : List (m a)) ->
    match ms with
    | Nil ->
      let dict : Monad m = monad.m in
      (match dict with
       | .Monad pure _ -> pure) @(List a) (Nil @a)
    | Cons m ms ->
      let dict : Monad m = monad.m in
      (match dict with
       | .Monad _ bind ->
         bind) @a @(List a) m (sequence.L2 @a @m monad.m ms)
traverse_.L1 : ∀a m. Monad m -> (a -> m Unit) -> a -> m Unit -> m Unit =
  fun @a @m (monad.m : Monad m) (f : a -> m Unit) (x : a) ->
    semi.L2 @Unit @m monad.m (f x)
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
foldableBinTree.foldr.L1 : ∀a b. (a -> b -> b) -> b -> BinTree a -> b =
  fun @a @b (f : a -> b -> b) (y0 : b) (t : BinTree a) ->
    match t with
    | Leaf -> y0
    | Branch l x r ->
      let dict : Foldable BinTree = foldableBinTree in
      (match dict with
       | .Foldable foldr _ ->
         foldr) @a @b f (f x (let dict : Foldable BinTree = foldableBinTree
                              in
                              (match dict with
                               | .Foldable foldr _ -> foldr) @a @b f y0 r)) l
foldableBinTree.foldl.L1 : ∀a b. (b -> a -> b) -> b -> BinTree a -> b =
  fun @a @b (f : b -> a -> b) (y0 : b) (t : BinTree a) ->
    match t with
    | Leaf -> y0
    | Branch l x r ->
      let dict : Foldable BinTree = foldableBinTree in
      (match dict with
       | .Foldable _ foldl ->
         foldl) @a @b f (f (let dict : Foldable BinTree = foldableBinTree in
                            (match dict with
                             | .Foldable _ foldl -> foldl) @a @b f y0 l) x) r
foldableBag.foldr.L1 : ∀a b. (a -> b -> b) -> b -> Bag a -> b =
  fun @a @b (f : a -> b -> b) (y0 : b) (bag : Bag a) ->
    let dict : Foldable BinTree = foldableBinTree in
    (match dict with
     | .Foldable foldr _ -> foldr) @a @b f y0 (coerce @(Bag -> _) bag)
foldableBag.foldl.L1 : ∀a b. (b -> a -> b) -> b -> Bag a -> b =
  fun @a @b (f : b -> a -> b) (y0 : b) (bag : Bag a) ->
    let dict : Foldable BinTree = foldableBinTree in
    (match dict with
     | .Foldable _ foldl -> foldl) @a @b f y0 (coerce @(Bag -> _) bag)
bag_insert.L1 : (∀_14. Ord _14 -> _14 -> BinTree _14 -> BinTree _14) -> (∀_14. Ord _14 -> _14 -> BinTree _14 -> BinTree _14) =
  fun (insert : ∀_14. Ord _14 -> _14 -> BinTree _14 -> BinTree _14) @_14 (ord._14 : Ord _14) (x : _14) (t : BinTree _14) ->
    match t with
    | Leaf -> Branch @_14 (Leaf @_14) x (Leaf @_14)
    | Branch l y r ->
      match let dict : Ord _14 = ord._14 in
            (match dict with
             | .Ord _ _ _ lt -> lt) x y with
      | False -> Branch @_14 l y (insert @_14 ord._14 x r)
      | True -> Branch @_14 (insert @_14 ord._14 x l) y r
tsort.L1 : Bag Int -> Int -> Bag Int =
  fun (s : Bag Int) (x : Int) ->
    let ord.a : Ord Int = ordInt
    and x : Int = x
    and s : Bag Int = s
    in
    let rec insert : ∀_14. Ord _14 -> _14 -> BinTree _14 -> BinTree _14 =
              bag_insert.L1 insert
    in
    coerce @(_ -> Bag) (insert @Int ord.a x (coerce @(Bag -> _) s))
main.L1 : List Int -> IO Unit =
  fun (xs : List Int) ->
    let monad.m : Monad IO = monadIO
    and foldable.t : Foldable List = foldableList
    and f : Int -> IO Unit = print
    in
    let dict : Foldable List = foldable.t in
    (match dict with
     | .Foldable foldr _ ->
       foldr) @Int @(IO Unit) (traverse_.L1 @Int @IO monad.m f) (let dict : Monad IO =
                                                                       monad.m
                                                                 in
                                                                 (match dict with
                                                                  | .Monad pure _ ->
                                                                    pure) @Unit Unit) (let xs : List Int =
                                                                                             xs
                                                                                       in
                                                                                       let foldable.t : Foldable Bag =
                                                                                             foldableBag
                                                                                       in
                                                                                       let dict : Foldable Bag =
                                                                                             foldable.t
                                                                                       in
                                                                                       (match dict with
                                                                                        | .Foldable foldr _ ->
                                                                                          foldr) @Int @(List Int) (Cons @Int) (Nil @Int) (let dict : Foldable List =
                                                                                                                                                foldableList
                                                                                                                                          in
                                                                                                                                          (match dict with
                                                                                                                                           | .Foldable _ foldl ->
                                                                                                                                             foldl) @Int @(Bag Int) tsort.L1 (coerce @(_ -> Bag) (Leaf @Int)) xs))
main.L2 : Int -> IO Unit =
  fun (n : Int) ->
    let dict : Monad IO = monadIO in
    (match dict with
     | .Monad _ bind ->
       bind) @(List Int) @Unit (sequence.L3 @Int @IO monadIO (replicate.L1 @(IO Int) n input)) main.L1
