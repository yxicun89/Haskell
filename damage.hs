damage ::(Fractional a) => a -> a -> a -> a -> [a]
ns = [0.85,0.86..1.0]
damage a b c d =[((((a*2/5)+2)*b*c/d)/50)+2)*n | n <- ns]