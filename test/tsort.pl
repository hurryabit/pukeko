data Bag a = BinTree a
data BinTree a =
       | Leaf
       | Branch (BinTree a) a (BinTree a)
data Bool =
       | False
       | True
data Char
data Choice a b =
       | First a
       | Second b
data Dict$Eq a =
       | Dict$Eq (a -> a -> Bool)
data Dict$Foldable t =
       | Dict$Foldable (∀a b. (a -> b -> b) -> b -> t a -> b) (∀a b. (b -> a -> b) -> b -> t a -> b)
data Dict$Functor f =
       | Dict$Functor (∀a b. (a -> b) -> f a -> f b)
data Dict$Monad m =
       | Dict$Monad (∀a. a -> m a) (∀a b. m a -> (a -> m b) -> m b)
data Dict$Monoid m =
       | Dict$Monoid m (m -> m -> m)
data Dict$Ord a =
       | Dict$Ord (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool)
data Dict$Ring a =
       | Dict$Ring (a -> a) (a -> a -> a) (a -> a -> a) (a -> a -> a)
data IO a = World -> Pair a World
data Int
data List a =
       | Nil
       | Cons a (List a)
data Option a =
       | None
       | Some a
data Pair a b =
       | Pair a b
data Unit =
       | Unit
data World =
       | World
external add_int : Int -> Int -> Int = "add"
external ge_int : Int -> Int -> Bool = "ge"
external geti : Unit -> Int = "geti"
external gt_int : Int -> Int -> Bool = "gt"
external le_int : Int -> Int -> Bool = "le"
external lt_int : Int -> Int -> Bool = "lt"
external mul_int : Int -> Int -> Int = "mul"
external neg_int : Int -> Int = "neg"
external puti : Int -> Unit = "puti"
external seq : ∀a b. a -> b -> b = "seq"
external sub_int : Int -> Int -> Int = "sub"
bag_empty : ∀a. Bag a = fun @a -> coerce @(_ -> Bag) (Leaf @a)
bag_insert$ll1 : ∀_12. (∀_12. Dict$Ord _12 -> _12 -> BinTree _12 -> BinTree _12) -> Dict$Ord _12 -> _12 -> BinTree _12 -> BinTree _12 =
  fun @_12 ->
    fun (insert : ∀_12. Dict$Ord _12 -> _12 -> BinTree _12 -> BinTree _12) (dict$Ord$_12 : Dict$Ord _12) (x : _12) (t : BinTree _12) ->
      match t with
      | Leaf @_12 -> Branch @_12 (Leaf @_12) x (Leaf @_12)
      | Branch @_12 l y r ->
        match lt$ll1 @_12 dict$Ord$_12 x y with
        | False -> Branch @_12 l y (insert @_12 dict$Ord$_12 x r)
        | True -> Branch @_12 (insert @_12 dict$Ord$_12 x l) y r
bag_insert$ll3 : ∀a. Dict$Ord a -> a -> Bag a -> Bag a =
  fun @a ->
    fun (dict$Ord$a : Dict$Ord a) (x : a) (s : Bag a) ->
      let rec insert : ∀_12. Dict$Ord _12 -> _12 -> BinTree _12 -> BinTree _12 =
                fun @_12 -> bag_insert$ll1 @_12 insert
      in
      coerce @(_ -> Bag) (insert @a dict$Ord$a x (coerce @(Bag -> _) s))
bind$ll1 : ∀m. Dict$Monad m -> (∀a b. m a -> (a -> m b) -> m b) =
  fun @m ->
    fun (dict : Dict$Monad m) ->
      match dict with
      | Dict$Monad @m _ bind -> bind
dict$Foldable$Bag : Dict$Foldable Bag =
  let foldr : ∀a b. (a -> b -> b) -> b -> Bag a -> b =
        fun @a @b -> dict$Foldable$Bag$ll1 @a @b
  and foldl : ∀a b. (b -> a -> b) -> b -> Bag a -> b =
        fun @a @b -> dict$Foldable$Bag$ll2 @a @b
  in
  Dict$Foldable @Bag foldr foldl
dict$Foldable$Bag$ll1 : ∀a b. (a -> b -> b) -> b -> Bag a -> b =
  fun @a @b ->
    fun (f : a -> b -> b) (y0 : b) (bag : Bag a) ->
      foldr$ll1 @BinTree dict$Foldable$BinTree @a @b f y0 (coerce @(Bag -> _) bag)
dict$Foldable$Bag$ll2 : ∀a b. (b -> a -> b) -> b -> Bag a -> b =
  fun @a @b ->
    fun (f : b -> a -> b) (y0 : b) (bag : Bag a) ->
      foldl$ll1 @BinTree dict$Foldable$BinTree @a @b f y0 (coerce @(Bag -> _) bag)
dict$Foldable$BinTree : Dict$Foldable BinTree =
  let foldr : ∀a b. (a -> b -> b) -> b -> BinTree a -> b =
        fun @a @b -> dict$Foldable$BinTree$ll1 @a @b
  and foldl : ∀a b. (b -> a -> b) -> b -> BinTree a -> b =
        fun @a @b -> dict$Foldable$BinTree$ll2 @a @b
  in
  Dict$Foldable @BinTree foldr foldl
dict$Foldable$BinTree$ll1 : ∀a b. (a -> b -> b) -> b -> BinTree a -> b =
  fun @a @b ->
    fun (f : a -> b -> b) (y0 : b) (t : BinTree a) ->
      match t with
      | Leaf @a -> y0
      | Branch @a l x r ->
        foldr$ll1 @BinTree dict$Foldable$BinTree @a @b f (f x (foldr$ll1 @BinTree dict$Foldable$BinTree @a @b f y0 r)) l
dict$Foldable$BinTree$ll2 : ∀a b. (b -> a -> b) -> b -> BinTree a -> b =
  fun @a @b ->
    fun (f : b -> a -> b) (y0 : b) (t : BinTree a) ->
      match t with
      | Leaf @a -> y0
      | Branch @a l x r ->
        foldl$ll1 @BinTree dict$Foldable$BinTree @a @b f (f (foldl$ll1 @BinTree dict$Foldable$BinTree @a @b f y0 l) x) r
dict$Foldable$List : Dict$Foldable List =
  let foldr : ∀a b. (a -> b -> b) -> b -> List a -> b =
        fun @a @b -> dict$Foldable$List$ll1 @a @b
  and foldl : ∀a b. (b -> a -> b) -> b -> List a -> b =
        fun @a @b -> dict$Foldable$List$ll2 @a @b
  in
  Dict$Foldable @List foldr foldl
dict$Foldable$List$ll1 : ∀a b. (a -> b -> b) -> b -> List a -> b =
  fun @a @b ->
    fun (f : a -> b -> b) (y0 : b) (xs : List a) ->
      match xs with
      | Nil @a -> y0
      | Cons @a x xs ->
        f x (foldr$ll1 @List dict$Foldable$List @a @b f y0 xs)
dict$Foldable$List$ll2 : ∀a b. (b -> a -> b) -> b -> List a -> b =
  fun @a @b ->
    fun (f : b -> a -> b) (y0 : b) (xs : List a) ->
      match xs with
      | Nil @a -> y0
      | Cons @a x xs ->
        foldl$ll1 @List dict$Foldable$List @a @b f (f y0 x) xs
dict$Monad$IO : Dict$Monad IO =
  let pure : ∀a. a -> IO a = fun @a -> dict$Monad$IO$ll2 @a
  and bind : ∀a b. IO a -> (a -> IO b) -> IO b =
        fun @a @b -> dict$Monad$IO$ll4 @a @b
  in
  Dict$Monad @IO pure bind
dict$Monad$IO$ll1 : ∀a. a -> World -> Pair a World =
  fun @a -> Pair @a @World
dict$Monad$IO$ll2 : ∀a. a -> IO a =
  fun @a -> fun (x : a) -> coerce @(_ -> IO) (dict$Monad$IO$ll1 @a x)
dict$Monad$IO$ll3 : ∀a b. IO a -> (a -> IO b) -> World -> Pair b World =
  fun @a @b ->
    fun (mx : IO a) (f : a -> IO b) (world0 : World) ->
      match coerce @(IO -> _) mx world0 with
      | Pair @a @World x world1 -> coerce @(IO -> _) (f x) world1
dict$Monad$IO$ll4 : ∀a b. IO a -> (a -> IO b) -> IO b =
  fun @a @b ->
    fun (mx : IO a) (f : a -> IO b) ->
      coerce @(_ -> IO) (dict$Monad$IO$ll3 @a @b mx f)
dict$Ord$Int : Dict$Ord Int =
  let ge : Int -> Int -> Bool = ge_int
  and gt : Int -> Int -> Bool = gt_int
  and le : Int -> Int -> Bool = le_int
  and lt : Int -> Int -> Bool = lt_int
  in
  Dict$Ord @Int ge gt le lt
dict$Ring$Int : Dict$Ring Int =
  let neg : Int -> Int = neg_int
  and add : Int -> Int -> Int = add_int
  and sub : Int -> Int -> Int = sub_int
  and mul : Int -> Int -> Int = mul_int
  in
  Dict$Ring @Int neg add sub mul
foldl$ll1 : ∀t. Dict$Foldable t -> (∀a b. (b -> a -> b) -> b -> t a -> b) =
  fun @t ->
    fun (dict : Dict$Foldable t) ->
      match dict with
      | Dict$Foldable @t _ foldl -> foldl
foldr$ll1 : ∀t. Dict$Foldable t -> (∀a b. (a -> b -> b) -> b -> t a -> b) =
  fun @t ->
    fun (dict : Dict$Foldable t) ->
      match dict with
      | Dict$Foldable @t foldr _ -> foldr
input : IO Int = io$ll2 @Unit @Int geti Unit
io$ll1 : ∀a b. (a -> b) -> a -> World -> Pair b World =
  fun @a @b ->
    fun (f : a -> b) (x : a) (world : World) ->
      let y : b = f x in
      seq @b @(Pair b World) y (Pair @b @World y world)
io$ll2 : ∀a b. (a -> b) -> a -> IO b =
  fun @a @b ->
    fun (f : a -> b) (x : a) -> coerce @(_ -> IO) (io$ll1 @a @b f x)
le$ll1 : ∀a. Dict$Ord a -> a -> a -> Bool =
  fun @a ->
    fun (dict : Dict$Ord a) ->
      match dict with
      | Dict$Ord @a _ _ le _ -> le
lt$ll1 : ∀a. Dict$Ord a -> a -> a -> Bool =
  fun @a ->
    fun (dict : Dict$Ord a) ->
      match dict with
      | Dict$Ord @a _ _ _ lt -> lt
main : IO Unit =
  bind$ll1 @IO dict$Monad$IO @Int @Unit input main$ll2
main$ll1 : List Int -> IO Unit =
  fun (xs : List Int) ->
    traverse_$ll2 @Int @IO @List dict$Monad$IO dict$Foldable$List print$ll1 (tsort$ll2 xs)
main$ll2 : Int -> IO Unit =
  fun (n : Int) ->
    bind$ll1 @IO dict$Monad$IO @(List Int) @Unit (sequence$ll3 @Int @IO dict$Monad$IO (replicate$ll1 @(IO Int) n input)) main$ll1
print$ll1 : Int -> IO Unit =
  fun (n : Int) -> io$ll2 @Int @Unit puti n
pure$ll1 : ∀m. Dict$Monad m -> (∀a. a -> m a) =
  fun @m ->
    fun (dict : Dict$Monad m) ->
      match dict with
      | Dict$Monad @m pure _ -> pure
replicate$ll1 : ∀a. Int -> a -> List a =
  fun @a ->
    fun (n : Int) (x : a) ->
      match le$ll1 @Int dict$Ord$Int n 0 with
      | False ->
        Cons @a x (replicate$ll1 @a (sub$ll1 @Int dict$Ring$Int n 1) x)
      | True -> Nil @a
semi$ll1 : ∀a m. m a -> Unit -> m a =
  fun @a @m -> fun (m2 : m a) (x : Unit) -> m2
semi$ll2 : ∀a m. Dict$Monad m -> m Unit -> m a -> m a =
  fun @a @m ->
    fun (dict$Monad$m : Dict$Monad m) (m1 : m Unit) (m2 : m a) ->
      bind$ll1 @m dict$Monad$m @Unit @a m1 (semi$ll1 @a @m m2)
sequence$ll1 : ∀a m. Dict$Monad m -> a -> List a -> m (List a) =
  fun @a @m ->
    fun (dict$Monad$m : Dict$Monad m) (x : a) (xs : List a) ->
      pure$ll1 @m dict$Monad$m @(List a) (Cons @a x xs)
sequence$ll2 : ∀a m. Dict$Monad m -> List (m a) -> a -> m (List a) =
  fun @a @m ->
    fun (dict$Monad$m : Dict$Monad m) (ms : List (m a)) (x : a) ->
      bind$ll1 @m dict$Monad$m @(List a) @(List a) (sequence$ll3 @a @m dict$Monad$m ms) (sequence$ll1 @a @m dict$Monad$m x)
sequence$ll3 : ∀a m. Dict$Monad m -> List (m a) -> m (List a) =
  fun @a @m ->
    fun (dict$Monad$m : Dict$Monad m) (ms : List (m a)) ->
      match ms with
      | Nil @(m a) -> pure$ll1 @m dict$Monad$m @(List a) (Nil @a)
      | Cons @(m a) m ms ->
        bind$ll1 @m dict$Monad$m @a @(List a) m (sequence$ll2 @a @m dict$Monad$m ms)
sub$ll1 : ∀a. Dict$Ring a -> a -> a -> a =
  fun @a ->
    fun (dict : Dict$Ring a) ->
      match dict with
      | Dict$Ring @a _ _ sub _ -> sub
to_list$ll1 : ∀a t. Dict$Foldable t -> t a -> List a =
  fun @a @t ->
    fun (dict$Foldable$t : Dict$Foldable t) ->
      foldr$ll1 @t dict$Foldable$t @a @(List a) (Cons @a) (Nil @a)
traverse_$ll1 : ∀a m. Dict$Monad m -> (a -> m Unit) -> a -> m Unit -> m Unit =
  fun @a @m ->
    fun (dict$Monad$m : Dict$Monad m) (f : a -> m Unit) (x : a) (m : m Unit) ->
      semi$ll2 @Unit @m dict$Monad$m (f x) m
traverse_$ll2 : ∀a m t. Dict$Monad m -> Dict$Foldable t -> (a -> m Unit) -> t a -> m Unit =
  fun @a @m @t ->
    fun (dict$Monad$m : Dict$Monad m) (dict$Foldable$t : Dict$Foldable t) (f : a -> m Unit) ->
      foldr$ll1 @t dict$Foldable$t @a @(m Unit) (traverse_$ll1 @a @m dict$Monad$m f) (pure$ll1 @m dict$Monad$m @Unit Unit)
tsort$ll1 : Bag Int -> Int -> Bag Int =
  fun (s : Bag Int) (x : Int) -> bag_insert$ll3 @Int dict$Ord$Int x s
tsort$ll2 : List Int -> List Int =
  fun (xs : List Int) ->
    to_list$ll1 @Int @Bag dict$Foldable$Bag (foldl$ll1 @List dict$Foldable$List @Int @(Bag Int) tsort$ll1 (bag_empty @Int) xs)
