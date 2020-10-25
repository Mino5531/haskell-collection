hanoiLoesen :: Int -> String -> String -> String -> [(String,String)]
hanoiLoesen n a b c = hanoiZuListe n a b c []

hanoiZuListe 0 _ _ _ l = l
hanoiZuListe n a b c l = hanoiZuListe(n-1) a c b ((a,b) : hanoiZuListe (n-1) c b a l)