factorial n = case n of 
                0 -> 1
                x -> x * factorial (n-1)