--Aufgabe 1
--a
--i
geradeZahlen list = [x| x<-list ,x `mod` 2==0]
--ii
ungeradeZahlen list = [x|x<-list,x `mod` 2 /=0]
--iii
durchSieben list = [x|x<-list,x `mod` 7 == 0]
--b
b1 = [(i,j,i*j)|i<-[0..100],j<-[0..100],i `mod` 2 ==0,j `mod` 3 == 0,i+j `mod` 7 == 0]

--Aufgabe 2
--a
getDiscriminant a b c = ((b/a/2)**2)-(c/a)
--b
--i
getLoesungZ a b c
    |(getDiscriminant a b c == 0) = 1
    |(getDiscriminant a b c < 0) = 0
    |(getDiscriminant a b c > 0) = 2
--ii
getLoesungS a b c
    |(getDiscriminant a b c == 0) = "Eine Nullstelle"
    |(getDiscriminant a b c < 0) = "Keine Nullstelle"
    |(getDiscriminant a b c > 0) = "Zwei Nullstellen"
--c
solveWithDiscriminant a b c = (-((b/a)/2) + sqrt (getDiscriminant a b c),-((b/a)/2) - sqrt (getDiscriminant a b c))
--d
a p =(-p/2)
b p q = sqrt ((p/2)*(p/2)-q)
solvepq p q = ((a p) +(b p q),(a p)-(b p q))
--e 
giveY a b c x = a*x**2 + b*x + c
--f
giveZeros a b c = concat [[x,y]|(x,y)<-[solveWithDiscriminant a b c]]
--g 
g [a,b] = [x*x+x+4|x<-[a..b]]
--h
f x = x**2+x+1 
wertetab f a b = [f x|x<-[a..b]]
--Aufgabe 3
--a 
belegung2 = [(x,y)|x<-[0..1],y<-[0..1]]
belegung3 = [(x,y,z)|x<-[0..1],y<-[0..1],z<-[0..1]]
belegung4 = [(x,y,z,a)|x<-[0..1],y<-[0..1],z<-[0..1],a<-[0..1]]
--b
und x y 
    |(x==y && x==1) = 1
    |otherwise = 0
nand  ::Int -> Int -> Int
nand x y
    |(x==y && x==1) = 0
    |otherwise = 1
or1 x y
    |x == 1 || y == 1 = 1
    |otherwise = 0
xor  ::Int -> Int -> Int
xor m e =nand (nand m (nand m e)) (nand (nand m e) e)

complicatedThing = [(x,y,z,xor (or1 (und x y) (und x z)) (und y z)) | (x,y,z) <- belegung3]