latter []     = []
latter (x:xs) = iter xs (tail xs)
  where
iter ps [] = ps
iter ps qs = iter (tail ps) (drop 2 qs)