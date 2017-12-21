external (-) = "sub"
external (*) = "mul"
external (<=) = "le"
let foldr f y0 xs =
      match xs with
      | Nil -> y0
      | Cons x xs -> f x (foldr f y0 xs)
let replicate n x =
      match (<=) n 0 with
      | False -> Cons x (replicate ((-) n 1) x)
      | True -> Nil
external return = "return"
external print = "print"
external input = "input"
external (>>=) = "bind"
let (;ll1) m2 _ = m2
let (;) m1 m2 = (>>=) m1 ((;ll1) m2)
let sequence_io$ll2 x xs = return (Cons x xs)
let sequence_io$ll1 ms x =
      (>>=) (sequence_io ms) (sequence_io$ll2 x)
let sequence_io ms =
      match ms with
      | Nil -> return Nil
      | Cons m ms -> (>>=) m (sequence_io$ll1 ms)
let iter_io$ll1 f x m = (;) (f x) m
let iter_io f = foldr (iter_io$ll1 f) (return Unit)
let id x = x
let cata fmap f x =
      match x with
      | Fix y -> f (fmap (cata fmap f) y)
let ana fmap f x = Fix (fmap (ana fmap f) (f x))
let mapFix bimap f x =
      match x with
      | Fix y -> Fix (bimap f (mapFix bimap f) y)
let bimapListF f g x =
      match x with
      | NilF -> NilF
      | ConsF y z -> ConsF (f y) (g z)
let mapFixList = mapFix bimapListF
let toList$ll1 x =
      match x with
      | NilF -> Nil
      | ConsF y ys -> Cons y ys
let toList = cata (bimapListF id) toList$ll1
let fromList$ll1 x =
      match x with
      | Nil -> NilF
      | Cons y ys -> ConsF y ys
let fromList = ana (bimapListF id) fromList$ll1
let main$ll3 x = (*) 2 x
let main$ll2 xs =
      iter_io print (toList (mapFixList main$ll3 (fromList xs)))
let main$ll1 n = (>>=) (sequence_io (replicate n input)) main$ll2
let main = (>>=) input main$ll1
