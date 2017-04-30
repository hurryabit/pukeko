external abort = "abort"
let (&&) x y = if x then y else False
let (||) x y = if x then True else y
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
      if (<=) n 0 then Nil else Cons x (replicate ((-) n 1) x)
external return = "return"
external print = "print"
external input = "input"
external (>>=) = "bind"
let sequence_io$2 x xs = return (Cons x xs)
let sequence_io$1 ms x = (>>=) (sequence_io ms) (sequence_io$2 x)
let sequence_io ms =
      match ms with
      | Nil -> return Nil
      | Cons m ms -> (>>=) m (sequence_io$1 ms)
let nats$1 nats_from n = Cons n (nats_from ((+) n 1))
let nats =
      let rec nats_from = nats$1 nats_from in
      nats_from 0
let pair op xs1 =
      match xs1 with
      | Nil -> Nil
      | Cons x1 xs2 ->
        match xs2 with
        | Nil -> xs1
        | Cons x2 xs3 -> Cons (op x1 x2) (pair op xs3)
let single i x = RmqNode i i x RmqEmpty RmqEmpty
let combine op t1 t2 =
      match t1 with
      | RmqEmpty -> abort
      | RmqNode s1 _ v1 _ _ ->
        match t2 with
        | RmqEmpty -> abort
        | RmqNode _ e2 v2 _ _ -> RmqNode s1 e2 (op v1 v2) t1 t2
let build$1 op run ts =
      match ts with
      | Nil -> abort
      | Cons t1 ts2 ->
        match ts2 with
        | Nil -> t1
        | Cons _ _ -> run (pair (combine op) ts)
let build op xs =
      let rec run = build$1 op run in
      run (zip_with single nats xs)
let query$1 aux one op q_hi q_lo t =
      match t with
      | RmqEmpty -> one
      | RmqNode t_lo t_hi value left right ->
        if (||) ((<) q_hi t_lo) ((>) q_lo t_hi) then
          one
        else
          if (&&) ((<=) q_lo t_lo) ((<=) t_hi q_hi) then
            value
          else
            op (aux left) (aux right)
let query one op q_lo q_hi =
      let rec aux = query$1 aux one op q_hi q_lo in
      aux
let infinity = 1000000000
let min x y = if (<=) x y then x else y
let replicate_io n act = sequence_io (replicate n act)
let main$5 lo t hi =
      let res = query infinity min lo hi t in
      print res
let main$4 t lo = (>>=) input (main$5 lo t)
let main$6 x = return Unit
let main$3 m xs =
      let t = build min xs in
      (>>=) (replicate_io m ((>>=) input (main$4 t))) main$6
let main$2 n m = (>>=) (replicate_io n input) (main$3 m)
let main$1 n = (>>=) input (main$2 n)
let main = (>>=) input main$1
