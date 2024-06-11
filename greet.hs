greet name = case name of 
               "Juan" -> let niceGreeting = "Hello! So very nice to see you," in niceGreet ++ "Juan!"
               "Fernando" -> let niceGreeting = "Hello! So very nice to see you," in niceGreet ++ "Fernando!"
                name -> let badGreeting = "Oh! Pfft. It's you." in badGreeting ++ name