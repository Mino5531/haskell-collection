--Insertionsort
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) 
    | x < y     = x:y:ys                
    | otherwise = y:(insert x ys)
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

--Quicksort
qsort []= []
qsort (x:xs) = qsort [y | y<-xs, y<x] ++ [y | y<-xs, y==x] ++ [x] ++ qsort [y | y<-xs, y>x]

--Mergesort
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

--Bubblesort
bubbleSort uList = foldr swapTill [] uList

swapTill x [] = [x]
swapTill x (y:xs) = min x y : swapTill (max x y) xs