triangle sort a b c 
  |c^2 < a^2 + b^2 = 2
  |c^2 == a^2 + b^2 = 3
  |c^2 > a^2 + b^2 = 4
  |otherwise = 1
  where
   sort a b c
  | c < a && c < b  = (c,x,y)
  | c > a && c > b = (x,y,c)
  | otherwise = (x,c,y)
  where
    (x, y) = if a < b then (a,b) else (b,a)