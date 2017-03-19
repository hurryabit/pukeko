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
prefix_semi$1 m2 x = m2
prefix_semi m1 m2 = m1>>=prefix_semi$1 m2
iter_io$1 f x m = f x;m
iter_io f = foldr (iter_io$1 f) (return Unit)
print_list = iter_io print
p = 100000007
mul_p x y = (x*y)%p
add_p x y = (x+y)%p
sum_p = foldl add_p 0
scanl$1 f scanl_f y0 xs =
  match xs with
  | Nil -> Nil
  | Cons x xs -> let y0 = f y0 x
                 in Cons y0 (scanl_f y0 xs)
scanl f =
  letrec scanl_f = scanl$1 f scanl_f
  in scanl_f
sols$1 xs = sum_p (zip_with mul_p sols xs)
sols$2 xs x = Cons x xs
sols = Cons 1 (map (sols$1) (scanl (sols$2) Nil sols))
main = print (nth sols 2000%p)