external print : Int -> IO Unit = "print"
let g : Int -> Int -> Int -> Int -> Int =
      fun (a : Int) (b : Int) (c : Int) (d : Int) -> a
let main : IO Unit = print (g 1 2 3 4)
