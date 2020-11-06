qsort []= []
qsort (x:xs) = qsort [y | y<-xs, y<x] ++ [y | y<-xs, y==x] ++ [x] ++ qsort [y | y<-xs, y>x]

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y    = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys

msort [] = []
msort xs = go [[x] | x <- xs]
go [a] = a
go xs = go (pairs xs)
pairs (a:b:t) = merge a b : pairs t
pairs t = t
