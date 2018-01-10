external print = "print"
external input = "input"
external (>>=) = "bind"
let (;ll1) = fun m2 x -> m2
let (;) = fun m1 m2 -> (>>=) m1 ((;ll1) m2)
let fst =
      fun p ->
        match p with
        | Pair x fst$pm1 -> x
let snd =
      fun p ->
        match p with
        | Pair snd$pm1 y -> y
let main$ll2 =
      fun x y ->
        let p = Pair x y in
        (;) (print (fst p)) (print (snd p))
let main$ll1 = fun x -> (>>=) input (main$ll2 x)
let main = (>>=) input main$ll1
