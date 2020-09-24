--Aufgabe 0
add5 x = x+5;
hoch3 x = x*x*x;
inc x = x+1;
ist_zweistellig x
    |x<=(-10) || x >=10 = True
    |otherwise = False

--Aufgabe 1a
bmi  ::Float -> Float -> Float
bmi gewicht groesse=gewicht/(groesse*groesse)

--Aufgabe 1b
bmi2  ::Float -> Float -> Int
bmi2 gewicht groesse=round (gewicht/((groesse/100)*(groesse/100)))

--Aufgabe 2
dreiMal  ::[Char] -> [Char]
dreiMal x = x++x++x;

--Aufgabe 3
baktFlaeche ausgangsflaeche vermehrungsrate t = ausgangsflaeche*(1+vermehrungsrate/100)**t;
--a) 0.6450199887427406 cm^2
--b) 15h
--c) 222,51 %
--d) 6d (5,12d)

--Aufgabe 4
und  ::Bool -> Bool -> Bool
und x y = (x==y && x == True)

oder  ::Bool -> Bool -> Bool
oder True True = True
oder True _ = True
oder _ True = True
oder False False = False

und2 x y 
    |(x==y && x==True) = True
    |otherwise = False

oder2 x y
    |x == True || y == True = True
    |otherwise = False

xor  ::Char -> Char -> Char
xor 'f' 'f' = 'f'
xor 'w' 'f' = 'w'
xor 'f' 'w' = 'w'
xor 'w' 'w' = 'f'

nand  ::Int -> Int -> Int
nand x y = fromEnum ((x==y && x == 1) == False)

und3  ::Int -> Int -> Int
und3 m e =nand (nand m e) (nand m e)

or3  ::Int -> Int -> Int
or3 m e =nand (nand m m) (nand e e)

xor3  ::Int -> Int -> Int
xor3 m e =nand (nand m (nand m e)) (nand (nand m e) e)