external (-) : Int -> Int -> Int = "sub"
external (*) : Int -> Int -> Int = "mul"
external (%) : Int -> Int -> Int = "mod"
external (<=) : Int -> Int -> Bool = "le"
let foldr$ll1 : ∀a b. (a -> b -> b) -> b -> List a -> b =
      fun @a @b ->
        fun (f : a -> b -> b) (y0 : b) (xs : List a) ->
          match xs with
          | Nil @a -> y0
          | Cons @a x xs -> f x (foldr @a @b f y0 xs)
let foldr : ∀a b. (a -> b -> b) -> b -> List a -> b =
      fun @a @b -> foldr$ll1 @a @b
let take$ll1 : ∀a. Int -> List a -> List a =
      fun @a ->
        fun (n : Int) (xs : List a) ->
          match (<=) n 0 with
          | False ->
            match xs with
            | Nil @a -> Nil @a
            | Cons @a x xs -> Cons @a x (take @a ((-) n 1) xs)
          | True -> Nil @a
let take : ∀a. Int -> List a -> List a = fun @a -> take$ll1 @a
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
let iter_io$ll1 : ∀a. (a -> IO Unit) -> a -> IO Unit -> IO Unit =
      fun @a ->
        fun (f : a -> IO Unit) (x : a) (m : IO Unit) -> (;) @Unit (f x) m
let iter_io$ll2 : ∀a. (a -> IO Unit) -> List a -> IO Unit =
      fun @a ->
        fun (f : a -> IO Unit) ->
          foldr @a @(IO Unit) (iter_io$ll1 @a f) (return @Unit Unit)
let iter_io : ∀a. (a -> IO Unit) -> List a -> IO Unit =
      fun @a -> iter_io$ll2 @a
let gen$ll1 : ∀a. (a -> a) -> a -> List a =
      fun @a -> fun (f : a -> a) (x : a) -> Cons @a x (gen @a f (f x))
let gen : ∀a. (a -> a) -> a -> List a = fun @a -> gen$ll1 @a
let main$ll1 : Int -> Int =
      fun (x : Int) -> (%) ((*) 91 x) 1000000007
let main$ll2 : Int -> IO Unit =
      fun (n : Int) ->
        (;) @Unit (print n) (iter_io @Int print (take @Int n (gen @Int main$ll1 1)))
let main : IO Unit = (>>=) @Int @Unit input main$ll2
