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
let main = sum (between 1 20) in
main
