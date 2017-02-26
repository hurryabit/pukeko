prime :: Int
prime = 1000000*1000000+39

add_mod_prime x y
  | z < prime = z
  | otherwise = z - prime
  where z = x+y

fibs = 0 : 1 : zipWith add_mod_prime fibs (tail fibs)

main = print (fibs !! 1500000)
