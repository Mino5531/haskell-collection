--Aufgabe 1
lltol z = concat z
--Aufgabe 2
flatten listOfTuples = concat [[b] | (a,b) <- listOfTuples]
--Aufgabe 3
teilerList n = [x |x <- [1..n],mod n x == 0]