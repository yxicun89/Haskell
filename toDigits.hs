toDigits :: Int -> [Int]
toDigits 0 = []
toDigits n = [n `mod` 10] ++ toDigits (n `div` 10)