width :: Int -> Int
width 0 = 0
width n = width (n `div` 2) + 1