f x u k | k == 0 = 1
        | k`mod`2 == 0 = f'
        | otherwise = (x*f')`mod`u
        where f' = f ((x*x)`mod`u) u (k`div`2)