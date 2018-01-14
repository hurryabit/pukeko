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
external return : ∀a. a -> IO a = "return"
let h : ∀a. a -> a = fun @a -> fun (u : a) -> u
let main : IO Unit = return @Unit (h @Unit (h @Unit Unit))
