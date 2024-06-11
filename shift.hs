nexts = zip ['A'..'Y'] ['B'..'Z'] ++ [('Z','A')]
shift xs = [snd n | x <- xs, n <- nexts, fst n == x]
順番大事　紙に方針の順序を書くといい！