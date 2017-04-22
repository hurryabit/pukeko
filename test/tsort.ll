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
and prefix_semi$1 m2 x = m2
and prefix_semi m1 m2 = m1>>=prefix_semi$1 m2
and iter_io$1 f x m = f x;m
and iter_io f = foldr (iter_io$1 f) (return Unit)
and print_list = iter_io print
and prime = 1000000*1000000+39
and double_mod_prime x =
      let y = 2*x in
      if y<prime then y else y-prime
and gen f x = Cons x (gen f (f x))
and powers = gen double_mod_prime 1
and sum = foldr prefix_add 0
and mul_mod_prime x y = (x*y)%prime
and sum_prod xs ys = sum (zip_with mul_mod_prime xs ys)
and hash xs = sum_prod xs powers%prime
and numbers$1 x = (2*x)%200003
and numbers = take 200002 (gen (numbers$1) 1)
and tsort$1 t x = insert_tree x t
and tsort xs = in_order (foldl (tsort$1) Leaf xs)
and main = print (hash (tsort numbers))
in
main
