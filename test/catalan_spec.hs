p = 100000007 :: Integer

sols = 1 : map (sum . zipWith (*) sols) (tail $ scanl (flip (:)) [] sols)

main = print $ (sols !! 2000) `mod` p
