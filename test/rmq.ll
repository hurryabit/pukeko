let rec not x = if x then False else True
and prefix_and x y = if x then y else False
and prefix_or x y = if x then True else y
and foldr f y0 xs =
      match xs with
      | Nil -> y0
      | Cons x xs -> f x (foldr f y0 xs)
and foldl f y0 xs =
      match xs with
      | Nil -> y0
      | Cons x xs -> foldl f (f y0 x) xs
and take n xs =
      if n<=0 then
        Nil
      else
        match xs with
        | Nil -> Nil
        | Cons x xs -> Cons x (take (n-1) xs)
and nth xs n =
      match xs with
      | Nil -> abort
      | Cons x xs -> if n<=0 then x else nth xs (n-1)
and zip_with f xs ys =
      match xs with
      | Nil -> Nil
      | Cons x xs ->
        match ys with
        | Nil -> Nil
        | Cons y ys -> Cons (f x y) (zip_with f xs ys)
and partition$1 p part_p xs =
      match xs with
      | Nil -> Pair Nil Nil
      | Cons x xs ->
        match part_p xs with
        | Pair ys zs ->
          if p x then Pair (Cons x ys) zs else Pair ys (Cons x zs)
and partition p xs =
      let rec part_p = partition$1 p part_p in
      part_p xs
and append xs ys =
      match xs with
      | Nil -> ys
      | Cons x xs -> Cons x (append xs ys)
and concat = foldr append Nil
and map f xs =
      match xs with
      | Nil -> Nil
      | Cons x xs -> Cons (f x) (map f xs)
and concat_map f xs = concat (map f xs)
and length$1 x l = 1+l
and length = foldr (length$1) 0
and replicate n x =
      if n<=0 then Nil else Cons x (replicate (n-1) x)
and insert_tree x t =
      match t with
      | Leaf -> Branch Leaf x Leaf
      | Branch l y r ->
        if x<=y then
          Branch (insert_tree x l) y r
        else
          Branch l y (insert_tree x r)
and in_order t =
      match t with
      | Leaf -> Nil
      | Branch l x r -> append (in_order l) (Cons x (in_order r))
and prefix_semi$1 m2 _ = m2
and prefix_semi m1 m2 = m1>>=prefix_semi$1 m2
and sequence_io$2 x xs = return (Cons x xs)
and sequence_io$1 ms x = sequence_io ms>>=sequence_io$2 x
and sequence_io ms =
      match ms with
      | Nil -> return Nil
      | Cons m ms -> m>>=sequence_io$1 ms
and iter_io$1 f x m = f x;m
and iter_io f = foldr (iter_io$1 f) (return Unit)
and print_list = iter_io print
and nats$1 nats_from n = Cons n (nats_from (n+1))
and nats =
      let rec nats_from = nats$1 nats_from in
      nats_from 0
and pair op xs1 =
      match xs1 with
      | Nil -> Nil
      | Cons x1 xs2 ->
        match xs2 with
        | Nil -> xs1
        | Cons x2 xs3 -> Cons (op x1 x2) (pair op xs3)
and single i x = RmqNode i i x RmqEmpty RmqEmpty
and combine op t1 t2 =
      match t1 with
      | RmqEmpty -> abort
      | RmqNode s1 _ v1 _ _ ->
        match t2 with
        | RmqEmpty -> abort
        | RmqNode _ e2 v2 _ _ -> RmqNode s1 e2 (op v1 v2) t1 t2
and build$1 op run ts =
      match ts with
      | Nil -> abort
      | Cons t1 ts2 ->
        match ts2 with
        | Nil -> t1
        | Cons _ _ -> run (pair (combine op) ts)
and build op xs =
      let rec run = build$1 op run in
      run (zip_with single nats xs)
and query$1 aux one op q_hi q_lo t =
      match t with
      | RmqEmpty -> one
      | RmqNode t_lo t_hi value left right ->
        if q_hi<t_lo||q_lo>t_hi then
          one
        else
          if q_lo<=t_lo&&t_hi<=q_hi then value else op (aux left) (aux right)
and query one op q_lo q_hi =
      let rec aux = query$1 aux one op q_hi q_lo in
      aux
and infinity = 1000000000
and min x y = if x<=y then x else y
and replicate_io n act = sequence_io (replicate n act)
and main$5 lo t hi =
      let res = query infinity min lo hi t in
      print res
and main$4 t lo = input>>=main$5 lo t
and main$6 x = return Unit
and main$3 m xs =
      let t = build min xs in
      replicate_io m (input>>=main$4 t)>>=main$6
and main$2 n m = replicate_io n input>>=main$3 m
and main$1 n = input>>=main$2 n
and main = input>>=main$1
in
main
