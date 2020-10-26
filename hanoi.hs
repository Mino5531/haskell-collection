hanoi :: Int -> String -> String -> String -> [(String,String)]

hanoi 0 _ _ _ = []
hanoi n source dest temp = hanoi (n-1) source temp dest ++ [(source,dest)] ++ hanoi (n-1) temp dest source