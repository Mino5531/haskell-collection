--Aufgabe 1
--13+42 = 55 (Addieren)
--2^10 = 1024 (Potenzieren)
--(+) 16 26 = 42 (Addieren)
--map (*2) [1,2,3] = [2,4,6] (Alle elemente in der Liste mit 2 multiplizieren)
--filter (>5) [8,4,1,6,10] = [8,6,10] (Alle elemente in der liste die größer als 5 sind ausgeben)

--Aufgabe 2
square ::Int ->Int
square x =x^2
--Fehler können auftreten wenn ein anderer Datentyp als Int in die Funktion gegeben wird.
--Verbessern indem man sich nicht nur auf den Datentyp Int beschränkt.
--Ja
--Vor dem gleich zeichen stehen der Funktionsname mit den Parametern die mit einem Leerzeichen vom Namen und untereinander getrennt werden (Funktionskopf).
--Nach dem gleich zeichen steht der Funktionskörper, also der teil der Ausgeführt wird.

--Aufgabe 3
strange ::Int ->Int
strange x
    |x >10=div(x+3)2
    |x <0=(div(x*x)3)+5
    |otherwise =square x