letrec foldr f y0 xs = if is_nil xs then y0 else f (hd xs) (foldr f y0 (tl xs)) in
letrec foldl f y0 xs = if is_nil xs then y0 else foldl f (f y0 (hd xs)) (tl xs) in
let sum = foldl (fun x y -> x+y) 0 in
letrec iter f x = cons x (iter f (f x)) in
letrec take_while p xs =
  if is_nil xs then
    nil
  else
    let x = hd xs and xs = tl xs in
    if p x then cons x (take_while p xs) else nil
in
let between i j = take_while (fun k -> k <= j) (iter (fun k -> k+1) i) in
letrec take n xs =
  if n <= 0 || is_nil xs then
    nil
  else
    cons (hd xs) (take (n-1) (tl xs))
in
letrec last xs =
  if is_nil xs then
    abort
  else
    let ys = tl xs in
    if is_nil ys then
    hd xs
  else
    last ys
in
letrec map f xs = if is_nil xs then nil else cons (f (hd xs)) (map f (tl xs)) in
letrec ints = cons 1 (map (fun x -> x+1) ints) in
letrec zip_with f xs ys =
  if is_nil xs || is_nil ys then
    nil
  else 
    cons (f (hd xs) (hd ys)) (zip_with f (tl xs) (tl ys))
in
letrec fibs = cons 0 (cons 1 (zip_with (fun x y -> x+y) fibs (tl fibs))) in
let main = foldr print (neg 1) (take 50 fibs) in
main
