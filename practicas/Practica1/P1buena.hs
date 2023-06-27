
media :: Fractional a => [a] -> a
media xs
	|length xs == 0 = 0
	|otherwise = sum xs/(fromIntegral(length xs))


bisiesto :: Integral a => a -> Bool
bisiesto x
	| mod x 4 == 0 && mod x 100 /= 0 = True
	| mod x 100 == 0 && mod x 400 == 0 = True
	|otherwise = False
	
bisiestoIf :: Integral a => a -> Bool
bisiestoIf x =	if mod x 4 == 0 then True
				else False
				
				
and :: a -> a -> a
and True True = True
and a True = a
and a False = False
and True a  = a
and False a = False

//estricto en argumento que una funcion f sea estricta en el primer argumento necesariamente hay q evaluar ese argumento para la solucion
//definimos f 3 x  = 8
//f (2+1) (1/0) -> es estricta en el primer argumento ya que evalua el primer argumento y directamente perezosamente evalua la funcion a 8, sin mirar el segundo argumento
//y pasaria lo mismo con los demas argumentos, es decir si es f x 3 = 8 miraria el segundo argumento a ver si es estricto( mira si el segundo argumento es 3), y si no es 3 ya evalua
//la expresion con las incognitas