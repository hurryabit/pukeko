external print : Int -> IO Unit = "print"
let g$ll1 : Int -> Int -> Int -> Int -> Int =
      fun (a : Int) (b : Int) (c : Int) (d : Int) -> a
let g : Int -> Int -> Int -> Int -> Int = g$ll1
let f$ll1 : Int -> Int -> Int -> Int -> Int = g
let f$ll2 : Int -> Int -> Int -> Int -> Int = f$ll1
let f$ll3 : Int -> Int -> Int -> Int -> Int = f$ll2
let f : Int -> Int -> Int -> Int -> Int = f$ll3
let main : IO Unit = print (f 1 2 3 4)
