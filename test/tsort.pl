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
        fun @a @b -> dict$Foldable$List$ll1 @a @b
  and foldl : ∀a b. (b -> a -> b) -> b -> List a -> b =
        fun @a @b -> dict$Foldable$List$ll2 @a @b
  in
  Dict$Foldable @List foldr foldl
dict$Monad$IO : Monad IO =
  let pure : ∀a. a -> IO a = fun @a -> dict$Monad$IO$ll2 @a
  and bind : ∀a b. IO a -> (a -> IO b) -> IO b =
        fun @a @b -> dict$Monad$IO$ll4 @a @b
  in
  Dict$Monad @IO pure bind
input : IO Int = io$ll2 @Unit @Int geti Unit
dict$Foldable$BinTree : Foldable BinTree =
  let foldr : ∀a b. (a -> b -> b) -> b -> BinTree a -> b =
        fun @a @b -> dict$Foldable$BinTree$ll1 @a @b
  and foldl : ∀a b. (b -> a -> b) -> b -> BinTree a -> b =
        fun @a @b -> dict$Foldable$BinTree$ll2 @a @b
  in
  Dict$Foldable @BinTree foldr foldl
dict$Foldable$Bag : Foldable Bag =
  let foldr : ∀a b. (a -> b -> b) -> b -> Bag a -> b =
        fun @a @b -> dict$Foldable$Bag$ll1 @a @b
  and foldl : ∀a b. (b -> a -> b) -> b -> Bag a -> b =
        fun @a @b -> dict$Foldable$Bag$ll2 @a @b
  in
  Dict$Foldable @Bag foldr foldl
bag_empty : ∀a. Bag a = fun @a -> coerce @(_ -> Bag) (Leaf @a)
main : IO Unit =
  bind$ll1 @IO dict$Monad$IO @Int @Unit input main$ll2
le$ll1 : ∀a. Ord a -> a -> a -> Bool =
  fun @a (dict : Ord a) ->
    match dict with
    | Dict$Ord @a _ _ le _ -> le
lt$ll1 : ∀a. Ord a -> a -> a -> Bool =
  fun @a (dict : Ord a) ->
    match dict with
    | Dict$Ord @a _ _ _ lt -> lt
sub$ll1 : ∀a. Ring a -> a -> a -> a =
  fun @a (dict : Ring a) ->
    match dict with
    | Dict$Ring @a _ _ sub _ -> sub
foldr$ll1 : ∀t. Foldable t -> (∀a b. (a -> b -> b) -> b -> t a -> b) =
  fun @t (dict : Foldable t) ->
    match dict with
    | Dict$Foldable @t foldr _ -> foldr
foldl$ll1 : ∀t. Foldable t -> (∀a b. (b -> a -> b) -> b -> t a -> b) =
  fun @t (dict : Foldable t) ->
    match dict with
    | Dict$Foldable @t _ foldl -> foldl
dict$Foldable$List$ll1 : ∀a b. (a -> b -> b) -> b -> List a -> b =
  fun @a @b (f : a -> b -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil @a -> y0
    | Cons @a x xs ->
      f x (foldr$ll1 @List dict$Foldable$List @a @b f y0 xs)
dict$Foldable$List$ll2 : ∀a b. (b -> a -> b) -> b -> List a -> b =
  fun @a @b (f : b -> a -> b) (y0 : b) (xs : List a) ->
    match xs with
    | Nil @a -> y0
    | Cons @a x xs ->
      foldl$ll1 @List dict$Foldable$List @a @b f (f y0 x) xs
to_list$ll1 : ∀a t. Foldable t -> t a -> List a =
  fun @a @t (dict$Foldable$t : Foldable t) ->
    foldr$ll1 @t dict$Foldable$t @a @(List a) (Cons @a) (Nil @a)
replicate$ll1 : ∀a. Int -> a -> List a =
  fun @a (n : Int) (x : a) ->
    match le$ll1 @Int dict$Ord$Int n 0 with
    | False ->
      Cons @a x (replicate$ll1 @a (sub$ll1 @Int dict$Ring$Int n 1) x)
    | True -> Nil @a
pure$ll1 : ∀m. Monad m -> (∀a. a -> m a) =
  fun @m (dict : Monad m) ->
    match dict with
    | Dict$Monad @m pure _ -> pure
bind$ll1 : ∀m. Monad m -> (∀a b. m a -> (a -> m b) -> m b) =
  fun @m (dict : Monad m) ->
    match dict with
    | Dict$Monad @m _ bind -> bind
semi$ll1 : ∀a m. m a -> Unit -> m a =
  fun @a @m (m2 : m a) (x : Unit) -> m2
semi$ll2 : ∀a m. Monad m -> m Unit -> m a -> m a =
  fun @a @m (dict$Monad$m : Monad m) (m1 : m Unit) (m2 : m a) ->
    bind$ll1 @m dict$Monad$m @Unit @a m1 (semi$ll1 @a @m m2)
sequence$ll1 : ∀a m. Monad m -> a -> List a -> m (List a) =
  fun @a @m (dict$Monad$m : Monad m) (x : a) (xs : List a) ->
    pure$ll1 @m dict$Monad$m @(List a) (Cons @a x xs)
sequence$ll2 : ∀a m. Monad m -> List (m a) -> a -> m (List a) =
  fun @a @m (dict$Monad$m : Monad m) (ms : List (m a)) (x : a) ->
    bind$ll1 @m dict$Monad$m @(List a) @(List a) (sequence$ll3 @a @m dict$Monad$m ms) (sequence$ll1 @a @m dict$Monad$m x)
sequence$ll3 : ∀a m. Monad m -> List (m a) -> m (List a) =
  fun @a @m (dict$Monad$m : Monad m) (ms : List (m a)) ->
    match ms with
    | Nil @(m a) -> pure$ll1 @m dict$Monad$m @(List a) (Nil @a)
    | Cons @(m a) m ms ->
      bind$ll1 @m dict$Monad$m @a @(List a) m (sequence$ll2 @a @m dict$Monad$m ms)
traverse_$ll1 : ∀a m. Monad m -> (a -> m Unit) -> a -> m Unit -> m Unit =
  fun @a @m (dict$Monad$m : Monad m) (f : a -> m Unit) (x : a) (m : m Unit) ->
    semi$ll2 @Unit @m dict$Monad$m (f x) m
traverse_$ll2 : ∀a m t. Monad m -> Foldable t -> (a -> m Unit) -> t a -> m Unit =
  fun @a @m @t (dict$Monad$m : Monad m) (dict$Foldable$t : Foldable t) (f : a -> m Unit) ->
    foldr$ll1 @t dict$Foldable$t @a @(m Unit) (traverse_$ll1 @a @m dict$Monad$m f) (pure$ll1 @m dict$Monad$m @Unit Unit)
dict$Monad$IO$ll1 : ∀a. a -> World -> Pair a World =
  fun @a -> Pair @a @World
dict$Monad$IO$ll2 : ∀a. a -> IO a =
  fun @a (x : a) -> coerce @(_ -> IO) (dict$Monad$IO$ll1 @a x)
dict$Monad$IO$ll3 : ∀a b. IO a -> (a -> IO b) -> World -> Pair b World =
  fun @a @b (mx : IO a) (f : a -> IO b) (world0 : World) ->
    match coerce @(IO -> _) mx world0 with
    | Pair @a @World x world1 -> coerce @(IO -> _) (f x) world1
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
print$ll1 : Int -> IO Unit =
  fun (n : Int) -> io$ll2 @Int @Unit puti n
dict$Foldable$BinTree$ll1 : ∀a b. (a -> b -> b) -> b -> BinTree a -> b =
  fun @a @b (f : a -> b -> b) (y0 : b) (t : BinTree a) ->
    match t with
    | Leaf @a -> y0
    | Branch @a l x r ->
      foldr$ll1 @BinTree dict$Foldable$BinTree @a @b f (f x (foldr$ll1 @BinTree dict$Foldable$BinTree @a @b f y0 r)) l
dict$Foldable$BinTree$ll2 : ∀a b. (b -> a -> b) -> b -> BinTree a -> b =
  fun @a @b (f : b -> a -> b) (y0 : b) (t : BinTree a) ->
    match t with
    | Leaf @a -> y0
    | Branch @a l x r ->
      foldl$ll1 @BinTree dict$Foldable$BinTree @a @b f (f (foldl$ll1 @BinTree dict$Foldable$BinTree @a @b f y0 l) x) r
dict$Foldable$Bag$ll1 : ∀a b. (a -> b -> b) -> b -> Bag a -> b =
  fun @a @b (f : a -> b -> b) (y0 : b) (bag : Bag a) ->
    foldr$ll1 @BinTree dict$Foldable$BinTree @a @b f y0 (coerce @(Bag -> _) bag)
dict$Foldable$Bag$ll2 : ∀a b. (b -> a -> b) -> b -> Bag a -> b =
  fun @a @b (f : b -> a -> b) (y0 : b) (bag : Bag a) ->
    foldl$ll1 @BinTree dict$Foldable$BinTree @a @b f y0 (coerce @(Bag -> _) bag)
bag_insert$ll1 : ∀_14. (∀_14. Ord _14 -> _14 -> BinTree _14 -> BinTree _14) -> Ord _14 -> _14 -> BinTree _14 -> BinTree _14 =
  fun @_14 (insert : ∀_14. Ord _14 -> _14 -> BinTree _14 -> BinTree _14) (dict$Ord$_14 : Ord _14) (x : _14) (t : BinTree _14) ->
    match t with
    | Leaf @_14 -> Branch @_14 (Leaf @_14) x (Leaf @_14)
    | Branch @_14 l y r ->
      match lt$ll1 @_14 dict$Ord$_14 x y with
      | False -> Branch @_14 l y (insert @_14 dict$Ord$_14 x r)
      | True -> Branch @_14 (insert @_14 dict$Ord$_14 x l) y r
bag_insert$ll2 : ∀a. Ord a -> a -> Bag a -> Bag a =
  fun @a (dict$Ord$a : Ord a) (x : a) (s : Bag a) ->
    let rec insert : ∀_14. Ord _14 -> _14 -> BinTree _14 -> BinTree _14 =
              fun @_14 -> bag_insert$ll1 @_14 insert
    in
    coerce @(_ -> Bag) (insert @a dict$Ord$a x (coerce @(Bag -> _) s))
tsort$ll1 : Bag Int -> Int -> Bag Int =
  fun (s : Bag Int) (x : Int) -> bag_insert$ll2 @Int dict$Ord$Int x s
tsort$ll2 : List Int -> List Int =
  fun (xs : List Int) ->
    to_list$ll1 @Int @Bag dict$Foldable$Bag (foldl$ll1 @List dict$Foldable$List @Int @(Bag Int) tsort$ll1 (bag_empty @Int) xs)
main$ll1 : List Int -> IO Unit =
  fun (xs : List Int) ->
    traverse_$ll2 @Int @IO @List dict$Monad$IO dict$Foldable$List print$ll1 (tsort$ll2 xs)
main$ll2 : Int -> IO Unit =
  fun (n : Int) ->
    bind$ll1 @IO dict$Monad$IO @(List Int) @Unit (sequence$ll3 @Int @IO dict$Monad$IO (replicate$ll1 @(IO Int) n input)) main$ll1
