prime :: Int
prime = 1000000*1000000+39

double_mod_prime x
  | y < prime = y
  | otherwise = y - prime
  where y = 2*x

mul_mod_prime x y = (x*y) `mod` prime

hash xs = sum (zipWith mul_mod_prime xs (iterate double_mod_prime 1)) `mod` prime

main = print $ hash [1..10006]
