hanoi :: Int -> String -> String -> String -> [(String,String)]

hanoi 0 _ _ _ = []
hanoi n source dest temp = hanoi (n-1) source temp dest ++ [(source,dest)] ++ hanoi (n-1) temp dest source

hanoi2 :: Int -> String -> String -> String -> IO()
hanoi2 n source dest temp
    |n==1=putStrLn $ "Move from "++source++" to "++dest
    |otherwise= do
        hanoi2 (n-1) source temp dest
        putStrLn $ "Move from "++source++" to "++dest
        hanoi2 (n-1) temp dest source