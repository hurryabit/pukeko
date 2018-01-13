external (-) : Int -> Int -> Int = "sub"
external (<) : Int -> Int -> Bool = "lt"
external (<=) : Int -> Int -> Bool = "le"
let foldr$ll1 : ∀a b. (a -> b -> b) -> b -> List a -> b =
      fun @a @b ->
        fun (f : a -> b -> b) (y0 : b) (xs : List a) ->
          match xs with
          | Nil @a -> y0
          | Cons @a x xs -> f x (foldr @a @b f y0 xs)
let foldr : ∀a b. (a -> b -> b) -> b -> List a -> b = foldr$ll1
let partition$ll1 : ∀a. (a -> Bool) -> (List a -> Pair (List a) (List a)) -> List a -> Pair (List a) (List a) =
      fun @a ->
        fun (p : a -> Bool) (part_p : List a -> Pair (List a) (List a)) (xs : List a) ->
          match xs with
          | Nil @a -> Pair @(List a) @(List a) (Nil @a) (Nil @a)
          | Cons @a x xs ->
            match part_p xs with
            | Pair @(List a) @(List a) ys zs ->
              match p x with
              | False -> Pair @(List a) @(List a) ys (Cons @a x zs)
              | True -> Pair @(List a) @(List a) (Cons @a x ys) zs
let partition$ll2 : ∀a. (a -> Bool) -> List a -> Pair (List a) (List a) =
      fun @a ->
        fun (p : a -> Bool) (xs : List a) ->
          let rec part_p : List a -> Pair (List a) (List a) =
                    partition$ll1 @a p part_p
          in
          part_p xs
let partition : ∀a. (a -> Bool) -> List a -> Pair (List a) (List a) =
      partition$ll2
let append$ll1 : ∀a. List a -> List a -> List a =
      fun @a ->
        fun (xs : List a) (ys : List a) ->
          match xs with
          | Nil @a -> ys
          | Cons @a x xs -> Cons @a x (append @a xs ys)
let append : ∀a. List a -> List a -> List a = append$ll1
let replicate$ll1 : ∀a. Int -> a -> List a =
      fun @a ->
        fun (n : Int) (x : a) ->
          match (<=) n 0 with
          | False -> Cons @a x (replicate @a ((-) n 1) x)
          | True -> Nil @a
let replicate : ∀a. Int -> a -> List a = replicate$ll1
external return : ∀a. a -> IO a = "return"
external print : Int -> IO Unit = "print"
external input : IO Int = "input"
external (>>=) : ∀a b. IO a -> (a -> IO b) -> IO b = "bind"
let (;ll1) : ∀a. IO a -> Unit -> IO a =
      fun @a -> fun (m2 : IO a) (x : Unit) -> m2
let (;ll2) : ∀a. IO Unit -> IO a -> IO a =
      fun @a ->
        fun (m1 : IO Unit) (m2 : IO a) -> (>>=) @Unit @a m1 ((;ll1) @a m2)
let (;) : ∀a. IO Unit -> IO a -> IO a = (;ll2)
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
let sequence_io : ∀a. List (IO a) -> IO (List a) = sequence_io$ll3
let iter_io$ll1 : ∀a. (a -> IO Unit) -> a -> IO Unit -> IO Unit =
      fun @a ->
        fun (f : a -> IO Unit) (x : a) (m : IO Unit) -> (;) @Unit (f x) m
let iter_io$ll2 : ∀a. (a -> IO Unit) -> List a -> IO Unit =
      fun @a ->
        fun (f : a -> IO Unit) ->
          foldr @a @(IO Unit) (iter_io$ll1 @a f) (return @Unit Unit)
let iter_io : ∀a. (a -> IO Unit) -> List a -> IO Unit = iter_io$ll2
let qsort$ll1 : Int -> Int -> Bool =
      fun (x : Int) (y : Int) -> (<) y x
let qsort$ll2 : List Int -> List Int =
      fun (xs : List Int) ->
        match xs with
        | Nil @Int -> Nil @Int
        | Cons @Int x xs ->
          match partition @Int (qsort$ll1 x) xs with
          | Pair @(List Int) @(List Int) ys zs ->
            append @Int (qsort ys) (Cons @Int x (qsort zs))
let qsort : List Int -> List Int = qsort$ll2
let main$ll1 : List Int -> IO Unit =
      fun (xs : List Int) -> iter_io @Int print (qsort xs)
let main$ll2 : Int -> IO Unit =
      fun (n : Int) ->
        (>>=) @(List Int) @Unit (sequence_io @Int (replicate @(IO Int) n input)) main$ll1
let main : IO Unit = (>>=) @Int @Unit input main$ll2
