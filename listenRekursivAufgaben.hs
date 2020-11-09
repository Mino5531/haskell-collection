--Aufgabe 1
-- a)
laenge :: Num p => [a] -> p
laenge [] = 0
laenge (_:xs) = 1 + laenge xs

-- b)
letztes :: [p] -> p
letztes [] = error"Can not give out last from empty list"
letztes (x:[]) = x
letztes (x:xs) = letztes (xs)

-- c)
anfang :: [a] -> [a]
anfang [] = error"Can not give out init from empty list"
anfang (_:[]) = []
anfang (x:xs) = x:anfang(xs)

-- d)
gibNtes :: (Eq t, Num t) => [p] -> t -> p
gibNtes (x:_) 0 = x 
gibNtes (_:xs) n = gibNtes xs (n-1)


--e)
verbinde :: [a] -> [a] -> [a]
verbinde liste [] = liste
verbinde liste liste2 = verbinde (letztes liste2:liste) (anfang liste2)

--Aufgabe 2
quadliste :: Num a => [a] -> [a]
quadliste [] = []
quadliste (x:xs) = x^2:quadliste xs

-- b)
istEnthalten :: Eq t => t -> [t] -> Bool
istEnthalten _ [] = False
istEnthalten x (y:ys)
 | x == y = True
 | otherwise = istEnthalten x ys

-- c)
delErstes :: Eq t => t -> [t] -> [t]
delErstes _ [] = []
delErstes x (y:ys)
 | x == y = ys
 | otherwise = y:delErstes x ys

-- d)
delElem :: Eq t => t -> [t] -> [t]
delElem _ [] = []
delElem x (y:ys)
 | x == y = delElem x ys
 | otherwise = y:delElem x ys

-- e)
alleGleich :: Eq t => t -> [t] -> Bool
alleGleich _ [] = True
alleGleich x (y:ys)
 | x == y = alleGleich x ys
 | otherwise = False

-- f)
streicheGleiche :: Eq a => [a] -> [a]
streicheGleiche [] = []
streicheGleiche (x:xs) = x:streicheGleiche (delElem x xs)


--Aufgabe 3
-- a)

vereinigung :: Eq a => [a] -> [a] -> [a]
vereinigung menge1 menge2 = streicheGleiche (menge1++menge2)

-- b)



durchschnitt :: (Fractional a, Eq a) => [a] -> [a] -> a
durchschnitt menge1 menge2 = realDurchschnitt (vereinigung menge1 menge2)/ fromIntegral (length(vereinigung menge1 menge2))



realDurchschnitt :: Num p => [p] -> p
realDurchschnitt [] = 0
realDurchschnitt (x:xs) = x + realDurchschnitt xs

--Aufgabe 4

-- a)
insert :: a -> [a] -> [a]
insert _ [x] = [x]
insert x (y:ys) = y:x: insert x ys

-- b)
replaceAllElement x y [] = []
replaceAllElement x y (m:xs)
    |x == m = [y] ++ replaceAllElement x y xs
    |otherwise = [m] ++ replaceAllElement x y xs

-- c)
verdoppleElem [] = []
verdoppleElem (x:xs) = x:x:verdoppleElem xs

-- d)
verdoppleListe [] = []
verdoppleListe (x:xs) = x:xs++[x]++xs++verdoppleListe []