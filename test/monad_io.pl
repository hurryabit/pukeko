type Unit =
       | Unit
type Pair a b =
       | Pair a b
type Bool =
       | False
       | True
type Int
external (-) : Int -> Int -> Int = "sub"
external (>=) : Int -> Int -> Bool = "ge"
external (>) : Int -> Int -> Bool = "gt"
type List a =
       | Nil
       | Cons a (List a)
type IO a
external return : ∀a. a -> IO a = "return"
external print : Int -> IO Unit = "print"
external input : IO Int = "input"
external (>>=) : ∀a b. IO a -> (a -> IO b) -> IO b = "bind"
(;ll1) : ∀a. IO a -> Unit -> IO a =
  fun @a -> fun (m2 : IO a) (x : Unit) -> m2
(;) : ∀a. IO Unit -> IO a -> IO a =
  fun @a ->
    fun (m1 : IO Unit) (m2 : IO a) -> (>>=) @Unit @a m1 ((;ll1) @a m2)
when : Bool -> IO Unit -> IO Unit =
  fun (p : Bool) (m : IO Unit) ->
    match p with
    | False -> return @Unit Unit
    | True -> m
count_down : Int -> IO Unit =
  fun (k : Int) ->
    when ((>=) k 0) ((;) @Unit (print k) (count_down ((-) k 1)))
repeat_m : Int -> IO Unit -> IO Unit =
  fun (k : Int) (m : IO Unit) ->
    when ((>) k 0) ((;) @Unit m (repeat_m ((-) k 1) m))
main$ll1 : Int -> Int -> IO Unit =
  fun (k : Int) (n : Int) -> repeat_m k (count_down n)
main$ll2 : Int -> IO Unit =
  fun (k : Int) -> (>>=) @Int @Unit input (main$ll1 k)
main : IO Unit = (>>=) @Int @Unit input main$ll2
