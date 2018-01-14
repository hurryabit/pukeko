type Unit =
       | Unit
type Pair a b =
       | Pair a b
type Bool =
       | False
       | True
type Int
type List a =
       | Nil
       | Cons a (List a)
type IO a
external print : Int -> IO Unit = "print"
let id : ∀a. a -> a =
      fun @a ->
        fun (x : a) ->
          let y : a = x in
          y
let main : IO Unit = print (id @Int 0)
