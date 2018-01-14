external print : Int -> IO Unit = "print"
let id : ∀a. a -> a =
      fun @a ->
        fun (x : a) ->
          let y : a = x in
          y
let main : IO Unit = print (id @Int 0)
