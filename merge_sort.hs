mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort (fst (halve xs))) (mergesort (snd (halve xs)))
  where merge [] [] = []
        merge xs [] = xs
        merge [] ys = ys
        merge (x:xs) (y:ys) 
          | x < y = [x] ++ merge xs (y:ys)
          | otherwise = [y] ++ merge (x:xs) ys
        halve xs 
          | even (length xs) = splitAt (length xs `div` 2) xs
          | otherwise = splitAt ((length xs `div` 2) + 1) xs
