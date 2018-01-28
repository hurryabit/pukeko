type Unit =
       | Unit
type Pair a b =
       | Pair a b
type Bool =
       | False
       | True
type Choice a b =
       | First a
       | Second b
type Int
type List a =
       | Nil
       | Cons a (List a)
type IO a
external print : Int -> IO Unit = "print"
external input : IO Int = "input"
external (>>=) : ∀a b. IO a -> (a -> IO b) -> IO b = "bind"
(;ll1) : ∀a. IO a -> Unit -> IO a =
  fun @a -> fun (m2 : IO a) (x : Unit) -> m2
(;) : ∀a. IO Unit -> IO a -> IO a =
  fun @a ->
    fun (m1 : IO Unit) (m2 : IO a) -> (>>=) @Unit @a m1 ((;ll1) @a m2)
type Option a =
       | None
       | Some a
fst : ∀a b. Pair a b -> a =
  fun @a @b ->
    fun (p : Pair a b) ->
      match p with
      | Pair @a @b x fst$pm1 -> x
snd : ∀a b. Pair a b -> b =
  fun @a @b ->
    fun (p : Pair a b) ->
      match p with
      | Pair @a @b snd$pm1 y -> y
main$ll1 : Int -> Int -> IO Unit =
  fun (x : Int) (y : Int) ->
    let p : Pair Int Int = Pair @Int @Int x y in
    (;) @Unit (print (fst @Int @Int p)) (print (snd @Int @Int p))
main$ll2 : Int -> IO Unit =
  fun (x : Int) -> (>>=) @Int @Unit input (main$ll1 x)
main : IO Unit = (>>=) @Int @Unit input main$ll2
