external print = "print"
external input = "input"
external (>>=) = "bind"
let (;1) m2 _ = m2
let (;) m1 m2 = (>>=) m1 ((;1) m2)
let fst p =
      match p with
      | Pair x _ -> x
let snd p =
      match p with
      | Pair _ y -> y
let main$2 x y =
      let p = Pair x y in
      (;) (print (fst p)) (print (snd p))
let main$1 x = (>>=) input (main$2 x)
let main = (>>=) input main$1
