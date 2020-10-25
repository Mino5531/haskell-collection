hanoiLoesen :: Int -> String -> String -> String -> [(String,String)]

hanoiLoesen 0 _ _ _ = []
hanoiLoesen n a b c = hanoiLoesen (n-1) a c b ++ [(a,b)] ++ hanoiLoesen (n-1) c b a