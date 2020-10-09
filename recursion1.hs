--Aufgabe 1
--a
multi n m
    | n>= 1 = m+multi (n-1) m
    |otherwise = 0

--b
teile a b
    | a < b = 0
    | a>= b = 1+teile (a-b) b

--Aufgabe 2
sumxy x y
    |x == y = y
    |otherwise = x `seq` x+sumxy (x+1) y

--Aufgabe 3
pot zahl exponent
    |exponent == 0 = 1
    |otherwise = zahl*pot zahl (exponent-1)

--Aufgabe 4
fakultaet  ::Integer -> Integer
fakultaet 0 = 1
fakultaet n = n*fakultaet(n-1)

--Aufgabe 5
ggt a 0 = a 
ggt a b = b `seq` ggt b (a `mod` b) where
