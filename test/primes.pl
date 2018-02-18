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
dict$Monoid$List : ∀a. Monoid (List a) =
  fun @a ->
    let empty : List a = Nil @a
    and append : List a -> List a -> List a = dict$Monoid$List$ll1 @a
    in
    Dict$Monoid @(List a) empty append
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
psums : List Int -> List Int =
  let rec psums0 : ∀_10. Ring _10 -> _10 -> List _10 -> List _10 =
            fun @_10 -> psums$ll1 @_10 psums0
  in
  psums0 @Int dict$Ring$Int 0
primes : List Int =
  Cons @Int 2 (Cons @Int 3 (sieve$ll2 (psums (Cons @Int 5 (repeat$ll1 @Int (Cons @Int 2 (Cons @Int 4 (Nil @Int))))))))
main : IO Unit =
  bind$ll1 @IO dict$Monad$IO @Int @Unit input main$ll1
eq$ll1 : ∀a. Eq a -> a -> a -> Bool =
  fun @a ->
    fun (dict : Eq a) ->
      match dict with
      | Dict$Eq @a eq -> eq
neq$ll1 : ∀a. Eq a -> a -> a -> Bool =
  fun @a ->
    fun (dict$Eq$a : Eq a) (x : a) (y : a) ->
      match eq$ll1 @a dict$Eq$a x y with
      | False -> True
      | True -> False
le$ll1 : ∀a. Ord a -> a -> a -> Bool =
  fun @a ->
    fun (dict : Ord a) ->
      match dict with
      | Dict$Ord @a _ _ le _ -> le
append$ll1 : ∀m. Monoid m -> m -> m -> m =
  fun @m ->
    fun (dict : Monoid m) ->
      match dict with
      | Dict$Monoid @m _ append -> append
add$ll1 : ∀a. Ring a -> a -> a -> a =
  fun @a ->
    fun (dict : Ring a) ->
      match dict with
      | Dict$Ring @a _ add _ _ -> add
sub$ll1 : ∀a. Ring a -> a -> a -> a =
  fun @a ->
    fun (dict : Ring a) ->
      match dict with
      | Dict$Ring @a _ _ sub _ -> sub
foldr$ll1 : ∀t. Foldable t -> (∀a b. (a -> b -> b) -> b -> t a -> b) =
  fun @t ->
    fun (dict : Foldable t) ->
      match dict with
      | Dict$Foldable @t foldr _ -> foldr
foldl$ll1 : ∀t. Foldable t -> (∀a b. (b -> a -> b) -> b -> t a -> b) =
  fun @t ->
    fun (dict : Foldable t) ->
      match dict with
      | Dict$Foldable @t _ foldl -> foldl
dict$Monoid$List$ll1 : ∀a. List a -> List a -> List a =
  fun @a ->
    fun (xs : List a) (ys : List a) ->
      foldr$ll1 @List dict$Foldable$List @a @(List a) (Cons @a) ys xs
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
nth_exn$ll1 : ∀a. List a -> Int -> a =
  fun @a ->
    fun (xs : List a) (n : Int) ->
      match xs with
      | Nil @a -> abort @a
      | Cons @a x xs ->
        match le$ll1 @Int dict$Ord$Int n 0 with
        | False -> nth_exn$ll1 @a xs (sub$ll1 @Int dict$Ring$Int n 1)
        | True -> x
bind$ll1 : ∀m. Monad m -> (∀a b. m a -> (a -> m b) -> m b) =
  fun @m ->
    fun (dict : Monad m) ->
      match dict with
      | Dict$Monad @m _ bind -> bind
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
io$ll1 : ∀a b. (a -> b) -> a -> World -> Pair b World =
  fun @a @b ->
    fun (f : a -> b) (x : a) (world : World) ->
      let y : b = f x in
      seq @b @(Pair b World) y (Pair @b @World y world)
io$ll2 : ∀a b. (a -> b) -> a -> IO b =
  fun @a @b ->
    fun (f : a -> b) (x : a) -> coerce @(_ -> IO) (io$ll1 @a @b f x)
print$ll1 : Int -> IO Unit =
  fun (n : Int) -> io$ll2 @Int @Unit puti n
repeat$ll1 : ∀a. List a -> List a =
  fun @a ->
    fun (xs : List a) ->
      let rec ys : List a =
                append$ll1 @(List a) (dict$Monoid$List @a) xs ys
      in
      ys
psums$ll1 : ∀_10. (∀_10. Ring _10 -> _10 -> List _10 -> List _10) -> Ring _10 -> _10 -> List _10 -> List _10 =
  fun @_10 ->
    fun (psums0 : ∀_10. Ring _10 -> _10 -> List _10 -> List _10) (dict$Ring$_10 : Ring _10) (n : _10) (xs : List _10) ->
      match xs with
      | Nil @_10 -> Nil @_10
      | Cons @_10 x xs ->
        let y : _10 = add$ll1 @_10 dict$Ring$_10 x n in
        Cons @_10 y (psums0 @_10 dict$Ring$_10 y xs)
filter$ll1 : ∀a. (a -> Bool) -> (List a -> List a) -> List a -> List a =
  fun @a ->
    fun (p : a -> Bool) (filter_p : List a -> List a) (xs : List a) ->
      match xs with
      | Nil @a -> Nil @a
      | Cons @a x xs ->
        let ys : List a = filter_p xs in
        match p x with
        | False -> ys
        | True -> Cons @a x ys
filter$ll2 : ∀a. (a -> Bool) -> List a -> List a =
  fun @a ->
    fun (p : a -> Bool) ->
      let rec filter_p : List a -> List a = filter$ll1 @a p filter_p in
      filter_p
sieve$ll1 : Int -> Int -> Bool =
  fun (p : Int) (k : Int) -> neq$ll1 @Int dict$Eq$Int (mod k p) 0
sieve$ll2 : List Int -> List Int =
  fun (ks : List Int) ->
    match ks with
    | Nil @Int -> abort @(List Int)
    | Cons @Int p ks ->
      Cons @Int p (sieve$ll2 (filter$ll2 @Int (sieve$ll1 p) ks))
main$ll1 : Int -> IO Unit =
  fun (n : Int) -> print$ll1 (nth_exn$ll1 @Int primes n)
