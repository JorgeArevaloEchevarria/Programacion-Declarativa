--Jorge Arevalo Echevarria
--Jesus Martin Moraleda

--Ejercicio 1

-- a)last
milast :: [a] -> a
milast [] = error "lista vacia"
milast (x:xs) = foldl (\x y -> y) x xs
-- b)reverse
mireverse :: [a] -> [a]
mireverse = foldl (\xs y -> y:xs) []

-- c) any
miany :: (a -> Bool) -> [a] -> Bool
miany p = foldr (\x y -> p x || y) False
-- all
--miall :: (a -> Bool) -> [a] -> Bool
--miall p = foldr (\x y -> p x && y) True
-- d) minimun
miminimum :: Ord a => [a] -> a
miminimum [] = error "lista vacia"
miminimum (x:xs) = foldl (\x y -> min x y) x xs
-- e) map
mimap :: (a->b) -> [a] -> [b]
mimap f = foldr (\x xs -> (f x):xs) []
-- filter
mifilter:: (a->Bool) -> [a] -> [a]
mifilter p = foldr (\x xs -> if p x then x:xs else xs) []
-- f)takewhile
mitakeWhile:: (a->Bool) -> [a] -> [a]
mitakeWhile p = foldr (\x xs -> if p x then x:xs else []) []


--Ejercicio 2
--Tres maneras(la c es la buena):
ej2a = concat [[k , (-(k+1))] | k <- [1,3..]]
ej2b = [x | xs <- [[k, (-(k+1))] | k <- [1,3..]], x <- xs]
ej2c :: [Int]
ej2c = foldl (\xs x -> xs ++ [x, (-(x+1))]) [] [1,3..100]
--recorrer al reves la lista para no usar "++" y usar ":" que es mas eficiente
ej2cOp :: [Int]
ej2cOp = foldl (\xs x -> ((x-1):(-x):xs)) [] [100,98..1]
--l2 = fold(\x xs -> if even x then (-x):xs else x:xs) [] [1..100]

--Ejercicio 3
ej3 :: [Int]
ej3 = foldl(\xs x -> xs ++ (replicate x x)) [] [1..]
--l3 = map(\n -> take n $ repeat n) [0..n]


--Ejercicio 4
nn:: [(Integer, Integer)]
nn = [(x, y) | z <- [0..], x <- [0..z], y <- [0..z], x+y == z]

--Ejercicio 5

intercala :: a -> [a] -> [[a]]
intercala x [] = [[x]]
intercala x (y:ys) = (x:y:ys) : [y:zs | zs <- intercala x ys]

permuta :: [a] -> [[a]]
permuta [] = [[]]
permuta (x:xs) = concat [intercala x ys | ys <- permuta xs]
--permuta' :: [a] -> [(a)]
--permuta' [] = [()]
--permuta' (x:xs) = [a++(x:b) | (a,b) <- [splitAt n ys | ys <- permuta' xs, n <- [0..length ys]]]


--Ejercicio 6 
--hacer solo un recorrido, 
--cuestaPOs :: Ord a =>[a]->[(a, Int)]
--cuestaPOs (y:xs) = ys where
	--(_,ys,_) = fold f (y,[],1)xs
	--f::Ord a =>(a,[(a,Int)],Int) -> a ->(a[(a,Int)],Int)
	--f(y,ps,i) x = if x > y then (x, ps++[(x,i)],i+1) else (x,ps,i+1)