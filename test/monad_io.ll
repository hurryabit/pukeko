not x = if x then False else True
prefix_and x y = if x then y else False
prefix_or x y = if x then True else y
prefix_semi$1 m2 x = m2
prefix_semi m1 m2 = m1>>=prefix_semi$1 m2
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
iter_m$1 f x m = f x;m
iter_m f = foldr (iter_m$1 f) (return Unit)
print_list = iter_m print
when p m = if p then m else return Unit
count_down k = when (k>=0) (print k;count_down (k-1))
repeat_m k m = when (k>0) (m;repeat_m (k-1) m)
main = repeat_m 3 (count_down 2)