external return : ∀a. a -> IO a = "return"
let h : ∀a. a -> a = fun @a -> fun (u : a) -> u
let main : IO Unit = return @Unit (h @Unit (h @Unit Unit))
