my_gteq :: Integer -> Integer -> Bool
my_gteq _ 0 = True
my_gteq 0 _ = False
my_gteq m n = my_gt (m-1) (n-1)