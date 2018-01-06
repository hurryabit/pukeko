external print = "print"
external input = "input"
external (>>=) = "bind"
let (;ll1) m2 x = m2
let (;) m1 m2 = (>>=) m1 ((;ll1) m2)
let fst p =
      match p with
      | Pair x _ -> x
let snd p =
      match p with
      | Pair _ y -> y
let main$ll2 x y =
      let p = Pair x y in
      (;) (print (fst p)) (print (snd p))
let main$ll1 x = (>>=) input (main$ll2 x)
let main = (>>=) input main$ll1
