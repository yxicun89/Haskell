clock a b 0 0 =  a - b
clock a b h 0 = sqrt ((a^2)+(b^2)-(2*a*b*cos((h/6)*pi)))
clock a b 0 m = sqrt ((a^2)+(b^2)-(2*a*b*cos((m/30)*pi)))
clock a b h m = sqrt ((a^2)+(b^2)-(2*a*b*cos(((h/6)-(m/30))*pi)))
clock a b h m = sqrt ((a*cos(