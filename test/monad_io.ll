external (-) = "sub"
external (>=) = "ge"
external (>) = "gt"
external return = "return"
external print = "print"
external input = "input"
external (>>=) = "bind"
let (;ll1) = fun m2 x -> m2
let (;) = fun m1 m2 -> (>>=) m1 ((;ll1) m2)
let when =
      fun p m ->
        match p with
        | False -> return Unit
        | True -> m
let count_down =
      fun k -> when ((>=) k 0) ((;) (print k) (count_down ((-) k 1)))
let repeat_m =
      fun k m -> when ((>) k 0) ((;) m (repeat_m ((-) k 1) m))
let main$ll2 = fun k n -> repeat_m k (count_down n)
let main$ll1 = fun k -> (>>=) input (main$ll2 k)
let main = (>>=) input main$ll1
