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
repeat xs =
  letrec ys = append xs ys
  in ys
psums$1 psums0 n xs =
  match xs with
  | Nil -> Nil
  | Cons x xs -> let y = x+n
                 in Cons y (psums0 y xs)
psums =
  letrec psums0 = psums$1 psums0
  in psums0 0
filter$1 filter_p p xs =
  match xs with
  | Nil -> Nil
  | Cons x xs -> let ys = filter_p xs
                 in if p x then Cons x ys else ys
filter p =
  letrec filter_p = filter$1 filter_p p
  in filter_p
sieve$1 p k = k%p!=0
sieve ks =
  match ks with
  | Nil -> abort
  | Cons p ks -> Cons p (sieve (filter (sieve$1 p) ks))
primes =
  Cons 2 (Cons 3 (sieve (psums (Cons 5 (repeat (Cons 2 (Cons 4 Nil)))))))
main = print (nth primes 5999)