n :: Int
biggers xs ys = [if xs !! n > ys !! n then xs !! n else ys !! n | n <- [1..length ys]]