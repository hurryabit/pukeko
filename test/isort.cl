letrec foldr f y0 xs =
  if is_nil xs then
    y0
  else f (hd xs) (foldr f y0 (tl xs))
in
letrec take n xs =
  if n <= 0 || is_nil xs then
    nil
  else
    cons (hd xs) (take (n-1) (tl xs))
in
let print_list = foldr print 0 in
letrec gen f x = cons x (gen f (f x)) in
let numbers = take 112 (gen (fun x -> (23*x) % 113) 1) in
letrec insert x xs =
  if is_nil xs || x <= hd xs then
    cons x xs
  else
    cons (hd xs) (insert x (tl xs))
in
letrec isort xs =
  if is_nil xs then
    nil
  else
    insert (hd xs) (isort (tl xs))
in
let main = print_list (isort numbers) in
main
