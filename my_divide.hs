my_divide :: Integer -> Integer -> Integer
my_divide m n
  | m < n     = 0
  | otherwise = my_divide (m - n) n + 1