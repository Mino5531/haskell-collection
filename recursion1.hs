--Aufgabe 1
--a
multi n m
    | n>= 1 = m+multi (n-1) m
    |otherwise = 0