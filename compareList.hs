compareList :: Ord a => [a] -> [a] -> Ordering
compareList [] [] = EQ
compareList [] _ = LT
compareList _ [] = GT
compareList (x:xs) (y:ys)
  | x > y = GT
  | x < y = LT
  | x == y = compareList xs ys 