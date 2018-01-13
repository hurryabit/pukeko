external (-) : Int -> Int -> Int = "sub"
external (>=) : Int -> Int -> Bool = "ge"
external (>) : Int -> Int -> Bool = "gt"
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
let when$ll1 : Bool -> IO Unit -> IO Unit =
      fun (p : Bool) (m : IO Unit) ->
        match p with
        | False -> return @Unit Unit
        | True -> m
let when : Bool -> IO Unit -> IO Unit = when$ll1
let count_down$ll1 : Int -> IO Unit =
      fun (k : Int) ->
        when ((>=) k 0) ((;) @Unit (print k) (count_down ((-) k 1)))
let count_down : Int -> IO Unit = count_down$ll1
let repeat_m$ll1 : Int -> IO Unit -> IO Unit =
      fun (k : Int) (m : IO Unit) ->
        when ((>) k 0) ((;) @Unit m (repeat_m ((-) k 1) m))
let repeat_m : Int -> IO Unit -> IO Unit = repeat_m$ll1
let main$ll1 : Int -> Int -> IO Unit =
      fun (k : Int) (n : Int) -> repeat_m k (count_down n)
let main$ll2 : Int -> IO Unit =
      fun (k : Int) -> (>>=) @Int @Unit input (main$ll1 k)
let main : IO Unit = (>>=) @Int @Unit input main$ll2
