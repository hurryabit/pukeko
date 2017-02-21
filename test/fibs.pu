letrec foldr f y0 xs = if is_nil xs then y0 else f (hd xs) (foldr f y0 (tl xs)) in
letrec take n xs =
  if n <= 0 || is_nil xs then
    nil
  else
    cons (hd xs) (take (n-1) (tl xs))
in
letrec zip_with f xs ys =
  if is_nil xs || is_nil ys then
    nil
  else
    cons (f (hd xs) (hd ys)) (zip_with f (tl xs) (tl ys))
in
let print_list = foldr print 0 in
letrec fibs = cons 0 (cons 1 (zip_with (fun x y -> x+y) fibs (tl fibs))) in
let main = print_list (take 93 fibs) in
main
