external abort = "abort"
let (&&) =
      fun x y ->
        match x with
        | False -> False
        | True -> y
let (||) =
      fun x y ->
        match x with
        | False -> y
        | True -> True
external (+) = "add"
external (-) = "sub"
external (<) = "lt"
external (<=) = "le"
external (>) = "gt"
let zip_with =
      fun f xs ys ->
        match xs with
        | Nil -> Nil
        | Cons x xs ->
          match ys with
          | Nil -> Nil
          | Cons y ys -> Cons (f x y) (zip_with f xs ys)
let replicate =
      fun n x ->
        match (<=) n 0 with
        | False -> Cons x (replicate ((-) n 1) x)
        | True -> Nil
external return = "return"
external print = "print"
external input = "input"
external (>>=) = "bind"
let sequence_io$ll2 = fun x xs -> return (Cons x xs)
let sequence_io$ll1 =
      fun ms x -> (>>=) (sequence_io ms) (sequence_io$ll2 x)
let sequence_io =
      fun ms ->
        match ms with
        | Nil -> return Nil
        | Cons m ms -> (>>=) m (sequence_io$ll1 ms)
let nats$ll1 = fun nats_from n -> Cons n (nats_from ((+) n 1))
let nats =
      let rec nats_from = nats$ll1 nats_from in
      nats_from 0
let pair =
      fun op xs1 ->
        match xs1 with
        | Nil -> Nil
        | Cons pair$pm1 pair$pm2 ->
          match pair$pm2 with
          | Nil -> xs1
          | Cons x2 xs3 -> Cons (op pair$pm1 x2) (pair op xs3)
let single = fun i x -> RmqNode i i x RmqEmpty RmqEmpty
let combine =
      fun op t1 t2 ->
        match t1 with
        | RmqEmpty -> abort
        | RmqNode s1 combine$pm1 v1 combine$pm2 combine$pm3 ->
          match t2 with
          | RmqEmpty -> abort
          | RmqNode combine$pm4 e2 v2 combine$pm5 combine$pm6 ->
            RmqNode s1 e2 (op v1 v2) t1 t2
let build$ll1 =
      fun op run ts ->
        match ts with
        | Nil -> abort
        | Cons build$pm1 build$pm2 ->
          match build$pm2 with
          | Nil -> build$pm1
          | Cons build$pm3 build$pm4 -> run (pair (combine op) ts)
let build =
      fun op xs ->
        let rec run = build$ll1 op run in
        run (zip_with single nats xs)
let query$ll1 =
      fun one op q_lo q_hi aux t ->
        match t with
        | RmqEmpty -> one
        | RmqNode t_lo t_hi value left right ->
          match (||) ((<) q_hi t_lo) ((>) q_lo t_hi) with
          | False ->
            match (&&) ((<=) q_lo t_lo) ((<=) t_hi q_hi) with
            | False -> op (aux left) (aux right)
            | True -> value
          | True -> one
let query =
      fun one op q_lo q_hi ->
        let rec aux = query$ll1 one op q_lo q_hi aux in
        aux
let infinity = 1000000000
let min =
      fun x y ->
        match (<=) x y with
        | False -> y
        | True -> x
let replicate_io = fun n act -> sequence_io (replicate n act)
let main$ll5 =
      fun t lo hi ->
        let res = query infinity min lo hi t in
        print res
let main$ll4 = fun t lo -> (>>=) input (main$ll5 t lo)
let main$ll6 = fun x -> return Unit
let main$ll3 =
      fun m xs ->
        let t = build min xs in
        (>>=) (replicate_io m ((>>=) input (main$ll4 t))) main$ll6
let main$ll2 = fun n m -> (>>=) (replicate_io n input) (main$ll3 m)
let main$ll1 = fun n -> (>>=) input (main$ll2 n)
let main = (>>=) input main$ll1
