--Jesus Martin Moraleda y Jorge Arevalo Echeverria

--Ejercicio 1
-- apartado a) La lista los n primeros n´umeros naturales pares, ordenada de menor a mayor, usando map.

lsPares:: Int -> [Int]
lsPares n = map (*2) [0..n]

--apartado b) La lista de pares formados por un n´umero natural como primera componente del par y su
--cuadrado como segunda, ordenados desde el n´umero natural n hasta 0: ([(n, n2), . . . ,(2, 4),(1, 1),(0, 0)]).

lsParCuadrados:: Int -> [(Int, Int)]
lsParCuadrados n = foldl (\xs x -> (x, x^2):xs) [][0..n]
--lsCuadrados:: Int -> [Int]
--lsCuadrados n = map (^2) [0..n]

--apartado c) La lista con las n primeras potencias de 3, usando iterate.
lsPot3 :: Int -> [Int]
lsPot3 n =take n (iterate (^3) 3)

--apartado d) La suma de los n´umeros menores que n que sean m´ultiplos de 3 o 5, usando fold
sumaMultiplos3o5 :: Int -> Int
--sumaMultiplos3o5 n = sum $ filter (\i-> mod i 3 == 0 || mod i 5 == 0)[3..n]

sumaMultiplos3o5 n
	| n == 0 = 0
	| mod n 3 == 0 = n + sumaMultiplos3o5(n-1)
	| mod n 5 == 0 = n + sumaMultiplos3o5(n-1)
    | otherwise = sumaMultiplos3o5(n-1)

-- apartado e) El primer n´umero primo mayor que n. Intenta hacerlo de varias formas.
	sigPrimos:: Integer -> Integer
	sigPrimos n = head ( dropWhile (not.primo) [n+1..])
	
	primo:: Integer -> Bool
	primo n
		| n < 2 = False
		| otherwise = divisores n == [1,n]
		
	divisores:: Integer -> [Integer]
	divisores n = filter (divisor n) [1..n]
		where divisor n m = mod n m == 0

--Ejercicio 2
--iguales f g n m ⇔ f x = g x, para todo n ≤ x ≤ m

iguales:: (Eq a2, Ord a1, Num a1) => (a1 -> a2) -> (a1 -> a2) -> a1 -> a1 -> Bool
iguales f g n m 
    | n <= m && (f n == g n)        = iguales f g (n+1) m
    | n <= m                        = False
    | n > m                         = True

--menorA n m p = menor x con n ≤ x ≤ m que verifica p

menorA :: (Ord t,Num t) => t -> t -> (t->Bool)-> t
menorA n m p | p n = n
             |(n>m) = -1
             | otherwise = menorA (n+1) m p

--menor n p = menor x con x ≥ n que verifica 
menor:: (Eq a, Num a) => a -> (a -> Bool) -> a
menor x p 
	| p x 					= x
	| otherwise				= menor (x+1) p

--mayorA n m p = mayor x con n ≤ x ≤ m que verifica p
--mayorA :: Enum a => a -> a -> (a -> Bool) -> abs
--mayorA n m p = last $ filter p [n..m]

--pt n m p = True si y solo si todos los x con n ≤ x ≤ m verifican p.
--pt:: Enum a => a -> a -> (a -> Bool) -> Bool
pt n m p = any p [n..m]

--Ejercicio 3
--filter2 xs p q = (us, vs) donde us son los elementos de xs que cumplen p y vs los que cumplen q

filter2 :: Num a => [a] -> (a -> Bool) -> (a -> Bool) -> ([a],[a])
filter2 [] _ _ = ([],[])
filter2 xs p q = let us = [i | i <- xs,p i] 
                     vs = [i | i <- xs,q i]
                 in (us,vs)

--partition p xs = (us, vs), donde us son los elementos de xs que cumplen p y vs son el resto.

partition :: (Num t,Ord t,Eq t) => (t -> Bool) -> [t] -> ([t],[t])
partition _ [] = ([],[])
partition p xs = let us = [i | i <- xs,p i]
                     vs = [i | i <- xs,not(p i)]
                     in (us,vs)

--mapx x [f0,f1,...,fn] = [f0 x,f1 x,...,fn x]

mapx :: (t->s) -> [t] -> [s]
mapx _ [] = []
mapx a (x:xs) = (a x):mapx a xs

--filter1 xss p = [ys1,...,ysn], donde si xss es la lista de listas [xs1,...,xsn], entonces para todo i, 1 ≤ i ≤ n, ysi es la lista con los elementos de la lista xsi que cumplen la propiedad p.
--filter1:: [a] -> [(a -> Bool)] -> [[a]]
--filter1 xs ps = map (\p -> filter p xs) ps


--filters xs ps = [xs1, . . . , xsn], donde xsi son los elementos de xs que cumplen pi supuesto que ps es [p1, . . . , pn].

filters :: (Ord t,Num t,Eq t) => [t] -> [(t->Bool)] -> [[t]]
filters _ [] = []
filters xs (x:ps) = [i | i <- xs,x i] : filters xs ps