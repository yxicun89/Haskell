mapp _ [] = []
mapp f (x:xs) = x : mapp f (map f xs)