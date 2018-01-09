external abort = "abort"
let (&&) x y =
      match x with
      | False -> False
      | True -> y
let (||) x y =
      match x with
      | False -> y
      | True -> True
external (+) = "add"
external (-) = "sub"
external (<) = "lt"
external (<=) = "le"
external (>) = "gt"
let zip_with f xs ys =
      match xs with
      | Nil -> Nil
      | Cons x xs ->
        match ys with
        | Nil -> Nil
        | Cons y ys -> Cons (f x y) (zip_with f xs ys)
let replicate n x =
      match (<=) n 0 with
      | False -> Cons x (replicate ((-) n 1) x)
      | True -> Nil
external return = "return"
external print = "print"
external input = "input"
external (>>=) = "bind"
let sequence_io$ll2 x xs = return (Cons x xs)
let sequence_io$ll1 ms x =
      (>>=) (sequence_io ms) (sequence_io$ll2 x)
let sequence_io ms =
      match ms with
      | Nil -> return Nil
      | Cons m ms -> (>>=) m (sequence_io$ll1 ms)
let nats$ll1 nats_from n = Cons n (nats_from ((+) n 1))
let nats =
      let rec nats_from = nats$ll1 nats_from in
      nats_from 0
let pair op xs1 =
      match xs1 with
      | Nil -> Nil
      | Cons pair$pm1 pair$pm2 ->
        match pair$pm2 with
        | Nil -> xs1
        | Cons x2 xs3 -> Cons (op pair$pm1 x2) (pair op xs3)
let single i x = RmqNode i i x RmqEmpty RmqEmpty
let combine op t1 t2 =
      match t1 with
      | RmqEmpty -> abort
      | RmqNode s1 _ v1 _ _ ->
        match t2 with
        | RmqEmpty -> abort
        | RmqNode _ e2 v2 _ _ -> RmqNode s1 e2 (op v1 v2) t1 t2
let build$ll1 op run ts =
      match ts with
      | Nil -> abort
      | Cons build$pm1 build$pm2 ->
        match build$pm2 with
        | Nil -> build$pm1
        | Cons _ _ -> run (pair (combine op) ts)
let build op xs =
      let rec run = build$ll1 op run in
      run (zip_with single nats xs)
let query$ll1 one op q_lo q_hi aux t =
      match t with
      | RmqEmpty -> one
      | RmqNode t_lo t_hi value left right ->
        match (||) ((<) q_hi t_lo) ((>) q_lo t_hi) with
        | False ->
          match (&&) ((<=) q_lo t_lo) ((<=) t_hi q_hi) with
          | False -> op (aux left) (aux right)
          | True -> value
        | True -> one
let query one op q_lo q_hi =
      let rec aux = query$ll1 one op q_lo q_hi aux in
      aux
let infinity = 1000000000
let min x y =
      match (<=) x y with
      | False -> y
      | True -> x
let replicate_io n act = sequence_io (replicate n act)
let main$ll5 t lo hi =
      let res = query infinity min lo hi t in
      print res
let main$ll4 t lo = (>>=) input (main$ll5 t lo)
let main$ll6 x = return Unit
let main$ll3 m xs =
      let t = build min xs in
      (>>=) (replicate_io m ((>>=) input (main$ll4 t))) main$ll6
let main$ll2 n m = (>>=) (replicate_io n input) (main$ll3 m)
let main$ll1 n = (>>=) input (main$ll2 n)
let main = (>>=) input main$ll1
