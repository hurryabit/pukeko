diff [] _  = []
diff xs [] = xs
diff (x:xs) (y:ys) =
  case x `compare` y of
    LT -> x : diff xs (y:ys)
    EQ -> diff xs ys
    GT -> diff (x:xs) ys

solve' [] = [[]]
solve' (ks:kss) = do
  k <- ks
  qs <- solve' $ zipWith (\ls i -> diff ls [k-i, k, k+i]) kss [1..]
  return $ k:qs

solve n = solve' (replicate n [1..n])

main = print $ length $ solve 12
