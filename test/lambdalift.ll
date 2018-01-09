external print = "print"
let g a b c d = a
let f$ll2 x y1 y2 z = g x y1 y2 z
let f$ll1 x y1 y2 = f$ll2 x y1 y2
let f x = f$ll1 x
let main = print (f 1 2 3 4)
