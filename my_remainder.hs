my_remainder :: Integer -> Integer -> Integer
my_remainder m n 
  | m < n = m
  | otherwise = my_remainder (m-n) n