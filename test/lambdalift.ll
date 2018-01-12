external print : Int -> IO Unit = "print"
let g$ll1 : Int -> Int -> Int -> Int -> Int =
      fun (a : Int) (b : Int) (c : Int) (d : Int) -> a
let g : Int -> Int -> Int -> Int -> Int = g$ll1
let f$ll1 : Int -> Int -> Int -> Int -> Int =
      fun (x : Int) (y1 : Int) (y2 : Int) (z : Int) -> g x y1 y2 z
let f$ll2 : Int -> Int -> Int -> Int -> Int =
      fun (x : Int) (y1 : Int) (y2 : Int) -> f$ll1 x y1 y2
let f$ll3 : Int -> Int -> Int -> Int -> Int =
      fun (x : Int) -> f$ll2 x
let f : Int -> Int -> Int -> Int -> Int = f$ll3
let main : IO Unit = print (f 1 2 3 4)
