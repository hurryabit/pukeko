not x = if x then False else True
prefix_and x y = if x then y else False
prefix_or x y = if x then True else y
foldr f y0 xs =
  match xs with
  | Nil -> y0
  | Cons x xs -> f x (foldr f y0 xs)
foldl f y0 xs =
  match xs with
  | Nil -> y0
  | Cons x xs -> foldl f (f y0 x) xs
take n xs =
  if n<=0 then Nil else match xs with
                        | Nil -> Nil
                        | Cons x xs -> Cons x (take (n-1) xs)
nth xs n =
  match xs with
  | Nil -> abort
  | Cons x xs -> if n<=0 then x else nth xs (n-1)
zip_with f xs ys =
  match xs with
  | Nil -> Nil
  | Cons x xs -> match ys with
                 | Nil -> Nil
                 | Cons y ys -> Cons (f x y) (zip_with f xs ys)
partition$1 p part_p xs =
  match xs with
  | Nil -> Pair Nil Nil
  | Cons x xs -> match part_p xs with
                 | Pair ys zs -> if p x then Pair (Cons x ys) zs else Pair ys (Cons x zs)
partition p xs =
  letrec part_p = partition$1 p part_p
  in part_p xs
append xs ys =
  match xs with
  | Nil -> ys
  | Cons x xs -> Cons x (append xs ys)
concat = foldr append Nil
map f xs =
  match xs with
  | Nil -> Nil
  | Cons x xs -> Cons (f x) (map f xs)
concat_map f xs = concat (map f xs)
length$1 x l = 1+l
length = foldr (length$1) 0
insert_tree x t =
  match t with
  | Leaf -> Branch Leaf x Leaf
  | Branch l y r -> if x<=y then Branch (insert_tree x l) y r else Branch l y (insert_tree x r)
in_order t =
  match t with
  | Leaf -> Nil
  | Branch l x r -> append (in_order l) (Cons x (in_order r))
prefix_semi$1 m2 x = m2
prefix_semi m1 m2 = m1>>=prefix_semi$1 m2
iter_io$1 f x m = f x;m
iter_io f = foldr (iter_io$1 f) (return Unit)
print_list = iter_io print
replicate k x = if k<=0 then Nil else Cons x (replicate (k-1) x)
diff xs ys =
  match xs with
  | Nil -> Nil
  | Cons x xs' -> match ys with
                  | Nil -> xs
                  | Cons y ys' -> if x<y then Cons x (diff xs' ys) else if x==y then diff xs' ys' else diff xs ys'
ints$1 go k = Cons k (go (k+1))
ints =
  letrec go = ints$1 go
  in go 1
solve_aux$2 k ls i = diff ls (Cons (k-i) (Cons k (Cons (k+i) Nil)))
solve_aux$1 kss k =
  map (Cons k) (solve_aux (zip_with (solve_aux$2 k) kss ints))
solve_aux kss =
  match kss with
  | Nil -> Cons Nil Nil
  | Cons ks kss -> concat_map (solve_aux$1 kss) ks
solve n = solve_aux (replicate n (take n ints))
main = print (length (solve 12))