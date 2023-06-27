--PracticaFinal PD - Jorge Arevalo Echevarria

-- declaramos el tipo de la signatura que admite el programa
data Prop = P|Q|R|S deriving (Show,Read,Eq)

-- declaramos el tipo de datos Formula, para representar las Formulas permitidas
data Formula = Atom Prop
			|Neg Formula
			|Conj Formula Formula
			|Disj Formula Formula
			deriving(Show,Read,Eq)
			
--declaramos Literal para tener los elementos de cada clausula
type Literal = Formula

--declaramos Clausula que contendra el conjunto de Literales que la forman
type Clausula = [Literal]

--tal que (literal f) se verifica si la fórmula F es un literal.
literal :: Formula -> Bool
literal (Atom p) = True
literal (Neg (Atom p)) = True
literal _ = False

{- prueba de salida de literal

	*Main> literal (Neg(Atom P))
	True
	
	*Main> literal (Disj(Atom P) (Atom Q))
	False
	
	*Main> literal (Atom P)
	True
-}


--Una Clausula es una disyuncion de literales, 
-- y que una formula esta en forma normal conjuntiva (FNC) si es conjuncion de clausulas.

--esClausula: dada una expresion de tipo Formula compruebe si se trata de una clausula o no.

esClausula :: Formula -> Bool
--esClausula (Atom p) = True
--esClausula (Neg f) = True
--esClausula (Conj f g) = True
esClausula (Disj f g)
	| literal(f) && literal(g) = True
	| literal(f) && not(literal(g)) = esClausula(g)
	| not(literal(f)) && literal(g) = esClausula(f)
	| not(literal(f)) && not(literal(g)) = esClausula(f) && esClausula(g)
esClausula _ = False

{-

*Main> esClausula(Atom P)
False
*Main> esClausula (Disj (Atom P) (Atom S))
True
*Main> esClausula (Disj (Atom P) (Disj (Atom Q)(Atom S)))
True
*Main> esClausula (Disj (Disj (Atom P)(Atom R)) (Disj (Atom Q)(Atom S)))
True
*Main> esClausula (Disj (Disj (Atom P)(Atom R)) (Conj (Atom Q)(Atom S)))
False
*Main> esClausula (Disj (Disj (Atom P)(Atom R)) (Disj (Neg(Atom Q))(Neg(Atom S))))
True

-}

--Forma Normal Conjuntiva (FNC):
-- • Literal: variable proposicional(Prop) P o ¬P.
-- • Una prop A esta en FNC si es una conjuncion
	-- C1 ∧ ... ∧ Cn
--donde cada clausula Ci es una disyuncion 
--Bi1 ∨ ... ∨ Bini y cada Bij es un literal.

--Metodo auxiliar que nos separara los Literales de la Clausula
clausula :: Formula -> Clausula
clausula f
	| literal f = [f]
clausula (Disj f g) = ((clausula f) ++ (clausula g))

--fncAlista: dada una expresion f de tipo Formula, que esta en FNC, devuelve
--una lista cuyos elementos son las clausulas que son las componentes de la conjuncion f. Es decir si
--F = C1 ∧ . . . ∧ Cn, el resultado ser´a [C1, . . . , Cn].
fncAlista :: Formula -> [Clausula]
fncAlista (Conj f g) = (fncAlista f) ++ (fncAlista g)
fncAlista f  = [clausula f]

{-

*Main> fncAlista (Conj (Atom P) (Atom Q))
[[Atom P],[Atom Q]]

*Main> fncAlista (Conj (Neg(Atom P)) (Atom Q))
[[Neg (Atom P)],[Atom Q]]

*Main> fncAlista (Conj (Neg(Atom P)) (Disj(Atom S)(Atom Q)))
[[Neg (Atom P)],[Atom S,Atom Q]]

*Main> fncAlista (Conj (Neg(Atom P)) (Conj(Atom S)(Atom Q)))
[[Neg (Atom P)],[Atom S],[Atom Q]]

*Main> fncAlista (Disj (Neg(Atom P)) (Conj(Atom S)(Atom Q)))
[[Neg (Atom P)*** Exception: C:\\Users\jorge\Desktop\PD\este año\Practica Final\PracticaFinal-JorgeArevaloEchevarria.hs:(90,1)-(92,52): Non-exhaustive patternsin function clausula

*Main> fncAlista (Conj ( Disj (Atom S) (Neg(Atom P)) ) (Conj(Atom S)(Atom Q)))
[[Atom S,Neg (Atom P)],[Atom S],[Atom Q]]

*Main> fncAlista (Conj ( Conj (Atom S) (Neg(Atom P)) ) (Conj(Atom S)(Atom Q)))
[[Atom S],[Neg (Atom P)],[Atom S],[Atom Q]]
-}


--clausulaLista: dada una expresion c de tipo Formula que es clausula,
--devuelva la lista de literales que forman la clausula. P v /Q v S es [P;/Q; S].

clausulaLista :: Formula ->[Literal]
clausulaLista (Disj f g)
	| literal(f) && literal(g) = [f] ++ [g]
	| esClausula(f) && esClausula(g) = clausulaLista(f) ++ clausulaLista(g)
	| esClausula(f) && literal(g) = clausulaLista(f) ++ [g]
	| literal(f) && esClausula(g) = [f] ++ clausulaLista(g)
	
	
	
{-
*Main> clausulaLista (Disj (Atom P) (Atom S))
[Atom P,Atom S]

*Main> clausulaLista (Disj (Atom P) (Disj (Atom Q)(Atom S)))
[Atom P,Atom Q,Atom S]

*Main> clausulaLista (Disj (Disj (Atom P)(Atom R)) (Disj (Atom Q)(Atom S)))
[Atom P,Atom R,Atom Q,Atom S]

*Main> clausulaLista (Disj (Disj (Atom P)(Atom R)) (Disj (Atom Q)(Atom S)))
[Atom P,Atom R,Atom Q,Atom S]

*Main> clausulaLista (Disj (Disj (Atom P)(Disj(Neg(Atom P))(Atom R))) (Disj (Atom Q)(Atom S)))
[Atom P,Neg (Atom P),Atom R,Atom Q,Atom S]

-}


	
-- esClausulaHorn que dada una expresion de tipo Formula que es clausula, compruebe
--si se trata de una clausula de Horn o no.
--En lógica proposicional, una fórmula lógica es una cláusula de Horn si es una
-- cláusula (disyunción de literales) con, como máximo, un literal positivo.


--metodo auxilar que nos dice si el Literal que entra por parametro es Positivo
esLiteralPos :: Literal -> Bool
esLiteralPos (Atom p) = True
esLiteralPos _ = False

--metodo auxilar en el que nos entra una lista de Literales,
-- y nos devuelve una lista con los Literales positivos
numLiteralesPos :: [Literal] -> [Literal]
numLiteralesPos [] = []
numLiteralesPos (x:xs)
	| (esLiteralPos x) = x : numLiteralesPos xs
	| otherwise = numLiteralesPos xs

esClausulaHorn :: Formula -> Bool
esClausulaHorn f = (length(numLiteralesPos (clausulaLista f))) < 2 

{-
*Main> esClausulaHorn (Disj (Atom P) (Atom S))
False

*Main> esClausulaHorn (Disj (Neg(Atom P)) (Atom S))
True

*Main> esClausulaHorn (Disj (Neg(Atom P)) (Disj (Atom S)(Atom R)))
False

*Main> esClausulaHorn (Disj (Neg(Atom P)) (Disj (Atom S)(Neg(Atom R))))
True

*Main> esClausulaHorn (Disj (Neg(Atom P)) (Disj (Disj(Atom P) (Atom S))(Neg(Atom R))))
False

*Main> esClausulaHorn (Disj (Neg(Atom P)) (Disj (Disj(Neg(Atom P)) (Atom S))(Neg(Atom R))))
True

-}
--metodo auxilar que nos elimina la primera aparicion del elemento en la lista
deleteAux :: (Eq a) => a -> [a] -> [a]
deleteAux y [] = []
deleteAux y (x:xs) = if x == y then xs else x : deleteAux y xs

{-
*Main> deleteAux (Neg(Atom S))  [Atom P,Neg (Atom P),Atom R,Atom Q,Atom S]
[Atom P,Neg (Atom P),Atom R,Atom Q,Atom S]

*Main> deleteAux (Atom S)  [Atom P,Neg (Atom P),Atom R,Atom Q,Atom S]
[Atom P,Neg (Atom P),Atom R,Atom Q]

*Main> deleteAux (Atom S)  [Atom S,Atom P,Neg (Atom P),Atom R,Atom Q,Atom S]
[Atom P,Neg (Atom P),Atom R,Atom Q,Atom S]
-}

--metodo auxilar que nos devuelve el Literal opuesto del Literal por parametro
opuesto :: Literal -> Literal
opuesto (Atom f) = Neg (Atom f)
opuesto (Neg (Atom f)) = Atom f

-- metodo auxilar nos devuelve si son opuestos los Literales de entrada por parametro 
sonOpuestos :: Literal -> Literal -> Bool
sonOpuestos a b
	| a == Neg(b) = True
	| Neg(a) == b = True
	| otherwise = False
	
--metodo auxilar que nos dice si el Literal tiene su opuesto en la lista de Literales	
hayOpuesto :: Literal -> [Literal] -> Bool
hayOpuesto (x) (y:ys)
	| (sonOpuestos(x)(y)) = True
	| ys == [] = False
	| (not(sonOpuestos(x)(y))) = hayOpuesto(x)(ys)
	| otherwise = False
	
	
{-

*Main> hayOpuesto (Atom P) [Atom P,Neg (Atom P),Atom R,Atom Q,Atom S]
True
*Main> hayOpuesto (Atom P) [Atom P,Atom R,Atom Q,Atom S]
False
	
-}

--metodo auxilar en el que elimina los elemtos que sean opuestos entre las dos listas 
-- y nos devuelve la union de las listas resultantes
quitarOpuestos :: [Literal] -> [Literal] -> [Literal]
quitarOpuestos (x:xs) (ys)
	| ys == [] = (x:xs)
	| not(hayOpuesto(x) (ys)) && xs == [] = x : ys
	| hayOpuesto(x)(ys) && xs == [] = deleteAux (opuesto x)(ys)
	| hayOpuesto(x)(ys) = quitarOpuestos(deleteAux (x)(xs))(deleteAux (opuesto x)(ys))
	| not(hayOpuesto(x) (ys)) = x : quitarOpuestos(xs)(ys)
	
{-
*Main> quitarOpuestos [Atom P,Atom Q,Atom S] [Neg (Atom P),Atom Q,Atom S]
[Atom Q,Atom S,Atom Q,Atom S]

*Main> quitarOpuestos [Atom P,Neg (Atom Q),Atom S] [Neg (Atom P),Atom Q,Atom S]
[Atom S,Atom S]

*Main> quitarOpuestos [Atom P,Neg (Atom Q),Atom S] [Neg (Atom P),Atom Q,Neg (Atom S)]
[]

*Main> quitarOpuestos [Atom P,Atom Q,Atom S] [Atom P,Atom Q,Atom S]
[Atom P,Atom Q,Atom S,Atom P,Atom Q,Atom S]
	
-}
-- resolvente xs1 xs2 que dadas dos expresiones xs1 y xs2, que son listas de literales
-- devuelva la lista de literales asociada al resolvente de dichas clausulas.	
resolvente :: Formula -> Formula -> [Literal]
resolvente f g = quitarOpuestos (clausulaLista(f)) (clausulaLista(g))

{-
*Main> resolvente (Disj (Atom P) (Atom S)) (Disj (Atom P) (Disj (Atom Q)(Atom S)))
[Atom P,Atom S,Atom P,Atom Q,Atom S]

*Main> resolvente (Disj (Atom P) (Atom S)) (Disj (Disj (Atom P)(Disj(Neg(Atom P))(Atom R))) (Disj (Atom Q)(Atom S)))
[Atom S,Atom P,Atom R,Atom Q,Atom S]

*Main> resolvente (Disj (Atom P) (Atom S)) (Disj (Neg(Atom P))(Neg(Atom S)))
[]

*Main> resolvente (Disj (Atom P) (Atom S)) (Disj (Neg(Atom P)) (Disj (Atom Q)(Neg(Atom S))))
[Atom Q]
-}

-- este caso para el enunciado que pone que xs1 y xs2 son listas de literales
resolvente2 :: [Literal] -> [Literal] -> [Literal]
resolvente2 f g = quitarOpuestos (f) (g)

{-
*Main> resolvente2 [Atom P,Atom S] [Atom P,Atom Q,Atom S]
[Atom P,Atom S,Atom P,Atom Q,Atom S]

*Main> resolvente2  [Atom P,Atom S] [Atom P,Neg (Atom P),Atom R,Atom Q,Atom S]
[Atom S,Atom P,Atom R,Atom Q,Atom S]

*Main> resolvente2  [Atom P,Atom S] [Neg (Atom P),Neg (Atom S)]
[]

*Main> resolvente2  [Atom P,Atom S] [Neg (Atom P),Atom Q,Neg (Atom S)]
[Atom Q] 
-}

-------------------------------------Segunda Parte-------------------------------------------------
{-
resolucion xs x = y, donde y es la lista de literales asociada a la clausula resultante de aplicar el algoritmo
anterior a la lista xs (que hace el papel del conjunto de clausulas de Horn S del algoritmo) y a x (que hace
el papel de la clausula de Horn G del algoritmo). Tanto las clausulas de xs como x vienen dadas mediante las
listas de literales asociadas a ellas. Por tanto, xs es una lista de listas de literales, mientras que x e y son listas
de literales.
-}
resolucion :: [[Literal]] -> [Literal] -> [Literal]
resolucion (xs) (g) = paso1 xs g  

--En cualquiera de los dos casos de parada, devuelve el G actual. 

--1. Se busca en S una clausula C que se pueda resolver con G.
--Si no existe, el algoritmo para.
--En caso contrario, se pasa al paso 2.
paso1 :: [[Literal]] -> [Literal] -> [Literal]
paso1 (x:xs) (g) 
	| ((x ++ g) == (resolvente2 x g)) && (xs == []) = g 
	| (x ++ g) == (resolvente2 x g) = paso1 (xs)(g)
	| (x ++ g) /= (resolvente2 x g) = paso2 (xs)(resolvente2 (x)(g)) 
	
--2. El nuevo G es res(G;C). Si este resolvente es vacio, el algoritmo termina .
-- En caso contrario vuelve al paso 1.	
paso2 :: [[Literal]] -> [Literal] -> [Literal]
paso2 (xs) (g)
	| g == [] = g
	| xs == [] = g
	| g /= [] = paso1 (xs) (g)
	

{-

*Main> resolucion [[Neg (Atom P),Atom S,Neg (Atom R)],[Atom P],[Neg(Atom S),Neg(Atom Q)],[Neg(Atom R),Atom R]]  [Neg(Atom P),Neg(Atom R)]
[Neg (Atom R)]

*Main> resolucion [[Neg (Atom P),Atom P],[Atom Q]] [Neg(Atom P),Neg(Atom Q)]
[Neg (Atom P)]

*Main> resolucion [[Neg (Atom P),Atom P],[Atom Q]] [Neg(Atom P)]
[Neg (Atom P)]

*Main> resolucion [[Atom P],[Atom Q]] [Neg(Atom P)]
[]

*Main>  resolucion [[Atom P],[Atom Q]] [Neg(Atom P),Neg(Atom Q)]
[]

*Main> resolucion [[Neg (Atom P),Atom P],[Atom Q]] [Neg(Atom P),Neg(Atom Q)]
[Neg (Atom P)]

*Main> resolucion [[Atom P,Atom R],[Atom Q],[Atom S]] [Neg(Atom P),Neg(Atom Q)]
[Atom R]

*Main> resolucion [[Neg (Atom P),Atom P],[Atom Q]] [Neg(Atom P),Neg(Atom S)]
[Neg (Atom P),Neg (Atom S)]

-}

-------------------------------------Tercera Parte-------------------------------------------------

--metodos auxiliares de cada funcion del programa para la implementacion de el menu

menuEsClausula :: IO ()
menuEsClausula = do
   putStr "Introduce la formula: "
   formula <- getLine
   let f = read formula :: Formula
   print (esClausula (f))
   

menuFncAlista :: IO ()
menuFncAlista  = do
   putStr "Introduce la formula: "
   formula <- getLine
   let f = read formula :: Formula
   print (fncAlista (f))

menuClausulaLista :: IO ()
menuClausulaLista  = do
   putStr "Introduce la formula: "
   formula <- getLine
   let f = read formula :: Formula
   print (clausulaLista (f))
   

menuEsClausulaHorn :: IO ()
menuEsClausulaHorn  = do
   putStr "Introduce la formula: "
   formula <- getLine
   let f = read formula :: Formula
   print (esClausulaHorn (f))
   
   
menuResolvente :: IO ()
menuResolvente = do
   putStr "Introduce la expresion1 como lista de Literales: "
   expresion1 <- getLine
   putStr "Introduce la expresion2 como lista de Literales: "
   expresion2 <- getLine
   let e1 = read expresion1 :: [Literal]
   let e2 = read expresion2 :: [Literal]
   print (resolvente2 (e1) (e2))
   
   
menuResolucion :: IO ()
menuResolucion = do
   putStr "Introduce el Conjunto de Clausulas de Horn como lista de listas de Literales: "
   conjunto1 <- getLine
   putStr "Introduce la Clausula de Horn sin Literal positivo como lista de Literales: "
   clausulaNeg <- getLine
   let cj1 = read conjunto1 :: [[Literal]]
   let cl2 = read clausulaNeg :: [Literal]
   print (resolucion (cj1) (cl2))
   



main :: IO ()
main = do 
	putStrLn "" -- para mejorar la vista
	putStrLn "Menu:"
	putStrLn "1. Comprobar una formula con el metodo esClausula. "
	putStrLn "2. Comprobar una formula con el metodo fncAlista. "
	putStrLn "3. Comprobar una formula con el metodo clausulaLista. "
	putStrLn "4. Comprobar una formula con el metodo esClausulaHorn. "
	putStrLn "5. Comprobar dos formulas con el metodo resolvente. "
	putStrLn "6. Resolucion de una Formula siendo clausula de Horn "
	putStrLn "7. Salir "
	putStrLn "Selecciona una de las opciones : "
	--hFlush stdout
	opcion <- getLine
	case opcion of
		"1" -> do 
				menuEsClausula 
				main
			
		"2" -> do 
				menuFncAlista
				main
		
		"3" -> do 
				menuClausulaLista
				main
		
		"4" -> do 
				menuEsClausulaHorn
				main
		
		"5" -> do 
				menuResolvente
				main
		
		"6" -> do 
				menuResolucion
				main
		
		"7" -> putStrLn "Saliendo del programa... "
		
		_ -> do
			putStrLn "Por favor elige una opcion valida"
			main

{-
Comprobacion de funcionamiento del menu

*Main> main

Menu:
1. Comprobar una formula con el metodo esClausula.
2. Comprobar una formula con el metodo fncAlista.
3. Comprobar una formula con el metodo clausulaLista.
4. Comprobar una formula con el metodo esClausulaHorn.
5. Comprobar dos formulas con el metodo resolvente.
6. Resolucion de una Formula siendo clausula de Horn
7. Salir
Selecciona una de las opciones :
1
Introduce la formula: (Disj (Atom P) (Disj (Atom Q)(Atom S)))
True

Menu:
1. Comprobar una formula con el metodo esClausula.
2. Comprobar una formula con el metodo fncAlista.
3. Comprobar una formula con el metodo clausulaLista.
4. Comprobar una formula con el metodo esClausulaHorn.
5. Comprobar dos formulas con el metodo resolvente.
6. Resolucion de una Formula siendo clausula de Horn
7. Salir
Selecciona una de las opciones :
2
Introduce la formula: (Conj (Neg(Atom P)) (Disj(Atom S)(Atom Q)))
[[Neg (Atom P)],[Atom S,Atom Q]]

Menu:
1. Comprobar una formula con el metodo esClausula.
2. Comprobar una formula con el metodo fncAlista.
3. Comprobar una formula con el metodo clausulaLista.
4. Comprobar una formula con el metodo esClausulaHorn.
5. Comprobar dos formulas con el metodo resolvente.
6. Resolucion de una Formula siendo clausula de Horn
7. Salir
Selecciona una de las opciones :
3
Introduce la formula: (Disj (Disj (Atom P)(Atom R)) (Disj (Atom Q)(Atom S)))
[Atom P,Atom R,Atom Q,Atom S]

Menu:
1. Comprobar una formula con el metodo esClausula.
2. Comprobar una formula con el metodo fncAlista.
3. Comprobar una formula con el metodo clausulaLista.
4. Comprobar una formula con el metodo esClausulaHorn.
5. Comprobar dos formulas con el metodo resolvente.
6. Resolucion de una Formula siendo clausula de Horn
7. Salir
Selecciona una de las opciones :
4
Introduce la formula: (Disj (Neg(Atom P)) (Disj (Disj(Atom P) (Atom S))(Neg(Atom R))))
False

Menu:
1. Comprobar una formula con el metodo esClausula.
2. Comprobar una formula con el metodo fncAlista.
3. Comprobar una formula con el metodo clausulaLista.
4. Comprobar una formula con el metodo esClausulaHorn.
5. Comprobar dos formulas con el metodo resolvente.
6. Resolucion de una Formula siendo clausula de Horn
7. Salir
Selecciona una de las opciones :
5
Introduce la expresion1 como lista de Literales: [Atom P,Atom S]
Introduce la expresion2 como lista de Literales: [Atom P,Neg (Atom P),Atom R,Atom Q,Atom S]
[Atom S,Atom P,Atom R,Atom Q,Atom S]

Menu:
1. Comprobar una formula con el metodo esClausula.
2. Comprobar una formula con el metodo fncAlista.
3. Comprobar una formula con el metodo clausulaLista.
4. Comprobar una formula con el metodo esClausulaHorn.
5. Comprobar dos formulas con el metodo resolvente.
6. Resolucion de una Formula siendo clausula de Horn
7. Salir
Selecciona una de las opciones :
6
Introduce el Conjunto de Clausulas de Horn como lista de listas de Literales: [[Neg (Atom P),Atom P],[Atom Q]]
Introduce la Clausula de Horn sin Literal positivo como lista de Literales: [Neg(Atom P),Neg(Atom Q)]
[Neg (Atom P)]

Menu:
1. Comprobar una formula con el metodo esClausula.
2. Comprobar una formula con el metodo fncAlista.
3. Comprobar una formula con el metodo clausulaLista.
4. Comprobar una formula con el metodo esClausulaHorn.
5. Comprobar dos formulas con el metodo resolvente.
6. Resolucion de una Formula siendo clausula de Horn
7. Salir
Selecciona una de las opciones :
7
Saliendo del programa...
*Main>

-}