halve :: [a] -> ([a], [a])
halve xs 
  | even (length xs) = splitAt (length xs `div` 2) xs
  | otherwise = splitAt ((length xs `div` 2) + 1) xs