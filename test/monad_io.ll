external (-) = "sub"
external (>=) = "ge"
external (>) = "gt"
external return = "return"
external print = "print"
external input = "input"
external (>>=) = "bind"
let (;ll1) m2 _ = m2
let (;) m1 m2 = (>>=) m1 ((;ll1) m2)
let when p m = if p then m else return Unit
let count_down k =
      when ((>=) k 0) ((;) (print k) (count_down ((-) k 1)))
let repeat_m k m = when ((>) k 0) ((;) m (repeat_m ((-) k 1) m))
let main$ll2 k n = repeat_m k (count_down n)
let main$ll1 k = (>>=) input (main$ll2 k)
let main = (>>=) input main$ll1
