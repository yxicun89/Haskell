my_gt :: Integer -> Integer -> Bool
my_gt _ 0 = True
my_gt 0 _ = False
my_gt m n = my_gt (m-1) (n-1)