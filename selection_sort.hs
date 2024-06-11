my_delete v [] = []
my_delete v (x:xs)
  | v == x = xs
  | otherwise = [x] ++ my_delete v xs

selection_sort [] = []
selection_sort xs = [m] ++ selection_sort xs1
  where
    m = minimum xs
    xs1 = my_delete m xs