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
prime = 1000000*1000000+39
double_mod_prime x =
  let y = 2*x
  in if y<prime then y else y-prime
gen f x = Cons x (gen f (f x))
powers = gen double_mod_prime 1
sum = foldr prefix_add 0
mul_mod_prime x y = (x*y)%prime
sum_prod xs ys = sum (zip_with mul_mod_prime xs ys)
hash xs = sum_prod xs powers%prime
numbers$1 x = (2*x)%200003
numbers = take 200002 (gen (numbers$1) 1)
qsort$1 x y = y<x
qsort xs =
  match xs with
  | Nil -> Nil
  | Cons x xs -> match partition (qsort$1 x) xs with
                 | Pair ys zs -> append (qsort ys) (Cons x (qsort zs))
main = print (hash (qsort numbers))