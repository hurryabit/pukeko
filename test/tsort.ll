external (-) : Int -> Int -> Int = "sub"
external (<=) : Int -> Int -> Bool = "le"
let foldr$ll1 : ∀a b. (a -> b -> b) -> b -> List a -> b =
      fun @a @b ->
        fun (f : a -> b -> b) (y0 : b) (xs : List a) ->
          match xs with
          | Nil @a -> y0
          | Cons @a x xs -> f x (foldr @a @b f y0 xs)
let foldr : ∀a b. (a -> b -> b) -> b -> List a -> b =
      fun @a @b -> foldr$ll1 @a @b
let foldl$ll1 : ∀a b. (b -> a -> b) -> b -> List a -> b =
      fun @a @b ->
        fun (f : b -> a -> b) (y0 : b) (xs : List a) ->
          match xs with
          | Nil @a -> y0
          | Cons @a x xs -> foldl @a @b f (f y0 x) xs
let foldl : ∀a b. (b -> a -> b) -> b -> List a -> b =
      fun @a @b -> foldl$ll1 @a @b
let append$ll1 : ∀a. List a -> List a -> List a =
      fun @a ->
        fun (xs : List a) (ys : List a) ->
          match xs with
          | Nil @a -> ys
          | Cons @a x xs -> Cons @a x (append @a xs ys)
let append : ∀a. List a -> List a -> List a =
      fun @a -> append$ll1 @a
let replicate$ll1 : ∀a. Int -> a -> List a =
      fun @a ->
        fun (n : Int) (x : a) ->
          match (<=) n 0 with
          | False -> Cons @a x (replicate @a ((-) n 1) x)
          | True -> Nil @a
let replicate : ∀a. Int -> a -> List a = fun @a -> replicate$ll1 @a
let insert_tree$ll1 : Int -> Tree Int -> Tree Int =
      fun (x : Int) (t : Tree Int) ->
        match t with
        | Leaf @Int -> Branch @Int (Leaf @Int) x (Leaf @Int)
        | Branch @Int l y r ->
          match (<=) x y with
          | False -> Branch @Int l y (insert_tree x r)
          | True -> Branch @Int (insert_tree x l) y r
let insert_tree : Int -> Tree Int -> Tree Int = insert_tree$ll1
let in_order$ll1 : ∀a. Tree a -> List a =
      fun @a ->
        fun (t : Tree a) ->
          match t with
          | Leaf @a -> Nil @a
          | Branch @a l x r ->
            append @a (in_order @a l) (Cons @a x (in_order @a r))
let in_order : ∀a. Tree a -> List a = fun @a -> in_order$ll1 @a
external return : ∀a. a -> IO a = "return"
external print : Int -> IO Unit = "print"
external input : IO Int = "input"
external (>>=) : ∀a b. IO a -> (a -> IO b) -> IO b = "bind"
let (;ll1) : ∀a. IO a -> Unit -> IO a =
      fun @a -> fun (m2 : IO a) (x : Unit) -> m2
let (;ll2) : ∀a. IO Unit -> IO a -> IO a =
      fun @a ->
        fun (m1 : IO Unit) (m2 : IO a) -> (>>=) @Unit @a m1 ((;ll1) @a m2)
let (;) : ∀a. IO Unit -> IO a -> IO a = fun @a -> (;ll2) @a
let sequence_io$ll1 : ∀a. a -> List a -> IO (List a) =
      fun @a ->
        fun (x : a) (xs : List a) -> return @(List a) (Cons @a x xs)
let sequence_io$ll2 : ∀a. List (IO a) -> a -> IO (List a) =
      fun @a ->
        fun (ms : List (IO a)) (x : a) ->
          (>>=) @(List a) @(List a) (sequence_io @a ms) (sequence_io$ll1 @a x)
let sequence_io$ll3 : ∀a. List (IO a) -> IO (List a) =
      fun @a ->
        fun (ms : List (IO a)) ->
          match ms with
          | Nil @(IO a) -> return @(List a) (Nil @a)
          | Cons @(IO a) m ms -> (>>=) @a @(List a) m (sequence_io$ll2 @a ms)
let sequence_io : ∀a. List (IO a) -> IO (List a) =
      fun @a -> sequence_io$ll3 @a
let iter_io$ll1 : ∀a. (a -> IO Unit) -> a -> IO Unit -> IO Unit =
      fun @a ->
        fun (f : a -> IO Unit) (x : a) (m : IO Unit) -> (;) @Unit (f x) m
let iter_io$ll2 : ∀a. (a -> IO Unit) -> List a -> IO Unit =
      fun @a ->
        fun (f : a -> IO Unit) ->
          foldr @a @(IO Unit) (iter_io$ll1 @a f) (return @Unit Unit)
let iter_io : ∀a. (a -> IO Unit) -> List a -> IO Unit =
      fun @a -> iter_io$ll2 @a
let tsort$ll1 : Tree Int -> Int -> Tree Int =
      fun (t : Tree Int) (x : Int) -> insert_tree x t
let tsort$ll2 : List Int -> List Int =
      fun (xs : List Int) ->
        in_order @Int (foldl @Int @(Tree Int) tsort$ll1 (Leaf @Int) xs)
let tsort : List Int -> List Int = tsort$ll2
let main$ll1 : List Int -> IO Unit =
      fun (xs : List Int) -> iter_io @Int print (tsort xs)
let main$ll2 : Int -> IO Unit =
      fun (n : Int) ->
        (>>=) @(List Int) @Unit (sequence_io @Int (replicate @(IO Int) n input)) main$ll1
let main : IO Unit = (>>=) @Int @Unit input main$ll2
