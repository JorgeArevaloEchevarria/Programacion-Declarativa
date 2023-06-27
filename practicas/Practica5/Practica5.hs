--Jorge Arevalo Echevarria

--Ejercicio 1
{-
import System.Random
juego1 :: IO ()

juego1 = do n <- randomIO :: IO Int
            putStrLn "Tienes que adivinar un numero entre 1 y 100"
            adivina n
-}
-- me he liado y he perdido mucho tiempo pensando que el numero n debia ser un aleatorio generado

adivina :: Int -> IO ()
adivina n = 
    do putStr "Escribe un numero: "
       c <- getLine
       let x = read c 
       case (compare x n) of
         LT -> do putStrLn " es bajo."
                  adivina n
         GT -> do putStrLn " es alto."
                  adivina n
         EQ -> putStrLn " Exactamente"
		 
--Ejercicio 2
--esta comentado ya que me producia error, y queria que compilase el primer ejercicio
type Fila = [Double]
type Matriz = [Fila]

--a)

--getDouble :: IO Double
--getDouble = do 
			--line <- getLine
			--return (read line::Double)

--getNumero :: (Floating a) => a -> [a] ->[a]
--getNumero x xs = (getDouble x)::xs


--getFila :: (Floating a) => Int -> [a] -> Fila
--getFila j xs = xs
--getFila j xs = getFila((j-1) getNumero x xs)

--getMatriz :: Int -> Int -> [Fila] -> IO Matriz
--getMatriz 0 j xs = xs
--getMatriz i j xs = getMatriz((i-1) (getFila j ys)::xs)

--b)

--dibujaMatriz :: Matriz -> IO ()
--dibujaMatriz = do
	--putStrLn "Introduce el numero de filas de la matriz:"
	--si <- getLine
	--i <- return (read sn::Int)
	--putStrLn "Introduce el numero de columnas de la matriz:"
	--sj <- getLine
	--j <- return (read sj::Int)
	--xs = getMatriz i j [[]] 
	--dibujaFila(xs)
	
	
	
--dibujaFila :: Fila -> IO()
--dibujaFila xs = print(x:xs) 
--				putStrLn(" ")	dibujaFila(xs)			
--dibujaFila []	

--Ejercicio 3

--formatea:: String -> String -> Int -> IO () 
--formatea fileIn fileOut n = do
	--input <- readFile fileIn
	--let ls = lines contents
		--output = zipWith (\n line -> n ++ " " ++ line) [0..] ls
	--writeFile fileOut output
	
