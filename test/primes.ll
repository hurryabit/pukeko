external abort = "abort"
external (+) = "add"
external (-) = "sub"
external (%) = "mod"
external (!=) = "ne"
external (<=) = "le"
let nth xs n =
      match xs with
      | Nil -> abort
      | Cons x xs -> if (<=) n 0 then x else nth xs ((-) n 1)
let append xs ys =
      match xs with
      | Nil -> ys
      | Cons x xs -> Cons x (append xs ys)
external print = "print"
external input = "input"
external (>>=) = "bind"
let repeat xs =
      let rec ys = append xs ys in
      ys
let psums$1 psums0 n xs =
      match xs with
      | Nil -> Nil
      | Cons x xs ->
        let y = (+) x n in
        Cons y (psums0 y xs)
let psums =
      let rec psums0 = psums$1 psums0 in
      psums0 0
let filter$1 filter_p p xs =
      match xs with
      | Nil -> Nil
      | Cons x xs ->
        let ys = filter_p xs in
        if p x then Cons x ys else ys
let filter p =
      let rec filter_p = filter$1 filter_p p in
      filter_p
let sieve$1 p k = (!=) ((%) k p) 0
let sieve ks =
      match ks with
      | Nil -> abort
      | Cons p ks -> Cons p (sieve (filter (sieve$1 p) ks))
let primes =
      Cons 2 (Cons 3 (sieve (psums (Cons 5 (repeat (Cons 2 (Cons 4 Nil)))))))
let main$1 n = print (nth primes n)
let main = (>>=) input main$1
