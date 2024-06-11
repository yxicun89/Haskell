bits 0 = 0
bits 1 = 1
bits n  
  | odd n = 1 + bits (n `div` 2)
  | otherwise = 0 + bits (n `div` 2)