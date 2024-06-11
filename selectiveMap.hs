selectiveMap p f [] = []
selectiveMap p f (x:xs)
  | p x = f x : selectiveMap p f xs
  | otherwise = x : selectiveMap p f xs