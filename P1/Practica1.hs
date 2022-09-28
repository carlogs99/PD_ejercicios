-- PD-Practica 1
-- Definiciones de funciones, tipos y clases.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- A continuación se importa el módulo QuickCheck. Necesita ser instalado
-- previamente con Cabal o Stack
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Evalúa las siguientes líneas para entender cómo funciona
-- el sistema de tipos que proporciona Haskell.
-- ---------------------------------------------------------------------

-- :type True
-- True :: Bool

-- :t True
-- True :: Bool

-- :t 1
-- 1 :: Num p => p

-- :t 1.1
-- 1.1 :: Fractional p => p

-- :t 'a'
-- 'a' :: Char

-- :t "a"
-- "a" :: [Char]

-- :t [1,2]
-- [1,2] :: Num a => [a]

-- :t [1,2.1]
-- [1,2.1] :: Fractional a => [a]

-- :t [1,'a']
-- Error, no se puede tener listas con distintos tipos

-- :t (1,'s')
-- (1,'s') :: Num a => (a, Char)

-- :t [[1],[1,2]]  
-- [[1],[1,2]] :: Num a => [[a]]

-- :t not
-- not :: Bool -> Bool

-- :t sum
-- sum :: Num a => [a] -> a
-- Mas generico que lista: sum :: (Foldable t, Num a) => t a -> a

-- :t (+)
-- (+) :: Num a => a -> a -> a 

-- :t []
-- [] :: [a]

-- :t ()
-- () :: ()

-- :t (3+)
-- (3+) :: Num a => a -> a 

-- :t length
-- length :: Foldable t => t a -> Int

-- :t zip
-- zip :: [a] -> [b] -> [(a,b)]

-- :t take
-- t :: Int -> [a] -> [a]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Sin evaluar las expresiones en GHC, decide qué tipo es  
-- el adecuado para cada una de ellas. Intenta dar el tipo más general.
-- ---------------------------------------------------------------------

-- i1 :: Integer  -- El primero va de regalo
-- i1 = 45

-- i2 :: String
-- i2 = "123"

-- i3 :: Bool
-- i3 = 45 <= i1

-- i4 :: Char
-- i4 = 'c'

-- i5 :: [[Char]]
-- i5 = ["abc","ok"]

-- i6 :: String
-- i6 = head i5

-- i7 :: String
-- i7 = tail "abc"

-- i8 :: (Bool, Float)
-- i8 = (True,4.5)

-- i9 :: [Integer]
-- i9 = [i1,34]

-- i10 :: Num a => [a] -> a
-- i10 = sum

-- i11 :: Integral a => a -> Int
-- i11 x = length [1..x]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Para cada una de las siguientes expresiones, reemplaza
-- undefined por una expresión válida para el tipo que la declara.
-- ---------------------------------------------------------------------

j1 :: (String,Integer)
j1 = ("hola", 1)

j2 :: [Integer]
j2 = [1..5]

j3 :: Char
j3 = '\n'

j4 :: Double
j4 = 1/3

j5 :: (Integer,String,Integer,Char)
j5 = (4, "", 3, 'a')

j6 :: ([Char],(Bool,String))
j6 = (['1', ' '], (True, "mundo"))

j7 :: [[Bool]]
j7 = [[False, True], [False], []]

j8 :: [(String,Bool)]
j8 = [("carlos", False)]

j9 :: Integer -> Integer
j9 n = 2 + n

j10 :: Float -> [Bool] -> Bool
j10 x bs = last bs

j11 :: [Char] -> [[Int]]
j11 xs = [[length xs]]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Conocemos el cambio actual del euro a dólares estadounidenses: 1
-- Euro son 1.17507 dólares

--   - Definir la constante tipoCambio con dicho valor.
--   - Calcular la expresiones para el cambio a dólares de distintas cantidades 
--     de euros y viceversa
-- ---------------------------------------------------------------------

tipoCambio :: Double
tipoCambio = 1.17507

variosCambios :: [(Double, Double)]
variosCambios = [(1, tipoCambio), (1/tipoCambio, 1), (0,0)]

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir dos funciones, aEuros y aDolares, que dada una cantidad de
-- dólares (resp. euros) permita obtener la cantidad de euros (resp.
-- dólares) equivalente. Volver a calcular los cambios anteriores utilizando las 
-- funciones definidas.

-- Nota: No es necesario redondear el resultado.
-- ---------------------------------------------------------------------

aEuros :: Double -> Double
aEuros x = x / tipoCambio

aDolares :: Double -> Double
aDolares x = x * tipoCambio

-- ---------------------------------------------------------------------
-- Ejercicio 6. Escribir la siguiente propiedad: dada cualquier cantidad de euros,
-- si la cambiamos a dólares y los dólares obtenidos los volvemos a
-- cambiar a euros, obtenemos la cantidad de euros original.

-- Nota: una propiedad es función que devuelve un booleano y su cuerpo
--       define una expresión para comprbar una propiedad.
-- ---------------------------------------------------------------------

propiedad_cambio :: Double -> Bool
propiedad_cambio x = (aEuros (aDolares x)) == x

-- ---------------------------------------------------------------------
-- Ejercicio 7. Si la propiedad anterior ha fallado analiza el posible problema y
-- busca una solución al mismo.
-- ---------------------------------------------------------------------

-- podria ser error de redondeo, chequear que esten cerca con valor absoluto
propiedad_cambio' x = (abs(aEuros (aDolares x) - x)) < 0.01

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir una función que determinar si una cadena de 
-- caracteres es palíndromo.
--
--
-- $ es_palindromo "anilina"
-- True
-- $ es_palindromo "dábale arroz a la zorra el abad"
-- True
-- $ es_palindromo []
-- False
-- $ es_palindromo "hola"
-- False

-- Nota: consideramos que la cadena vacía no es palíndromo.
-- ---------------------------------------------------------------------

mitad :: String -> String
mitad x = take ((length x) `div` 2) x 

es_palindromo :: String -> Bool
es_palindromo x = (mitad x) == (mitad (reverse x))

-- ---------------------------------------------------------------------
-- Ejercicio 9. Crear una función que genere la letra de DNI. 
--    Para calcular la letra del DNI o caracter de verificación solo debes 
--    de realizar los siguientes pasos:

--    1. Dividir la parte numérica del DNI entre el número 23.
--    2. Tomamos el resto de dicha división y buscamos la lista de 
--       letras
-- ---------------------------------------------------------------------

letrasDNI = "TRWAGMYFPDXBBJZSQVHLCKE"

letraDNI n = letrasDNI !! (n `mod` 23)

-- ---------------------------------------------------------------------
-- Ejercicio 10.  Conocemos que 0ºC se corresponden con 32ªF y que un incremento de
-- 5ºC suponen un incremento de 9ºF.

-- Definir una función que permita pasar de ºC a ºF (y otra para el
-- cambio contrario).

-- Si para mañana está prevista un mínimo de 19ºC y un máximo de
-- 34ºC, ¿cuál sería el rango expresado en ºF?
-- ---------------------------------------------------------------------

c2f x = x*(9/5) + 32

f2c x = (x - 32)*(5/9)

-- *Main> map c2f [19,34]
-- [66.2,93.2]

-- ---------------------------------------------------------------------
-- Ejercicio 11. Una tienda vende las mallas de 2kg de patatas a 2.70 euros. Para 
--  favorecer la venta de cantidades mayores ofrece un precio reducido
--  de 2.20 euros a partir de la quinta malla. Es decir, si un cliente
--  compra 18 mallas, las cinco primeras las cobra a 2.70 y las 13
--  restantes a 2.20.

--  * Definir una función que, dada la cantidad de mallas calcule el
--    precio sin tener en cuenta la promoción. Calcular el precio del
--    ejemplo proporcionado.

--  * Definir una función que, dada la cantidad de mallas, calcular el
--    precio correspondiente según la promoción. Usar dicha función
--    para calcular, de nuevo, el precio del ejemplo.

--  La oferta ha tenido tanto éxito que el vendedor decide ampliarla
--  reduciendo el precio a 2 euros a partir de la décima malla.

--  * Definir una función para la nueva promoción y volver al calcular
--    el precio del ejemplo.
-- ---------------------------------------------------------------------

precioNormal = 2.7
precioDescuento5 = 2.2
precioDescuento10 = 2

precioPatatas1 = (precioNormal*)

precioPatatas2 n = 
    if n <= 5 
        then n*precioNormal
        else 5*precioNormal + (n-5)*precioDescuento5

--alternativa sin if-else:

precioPatatas2' n =  (min n 5)*precioNormal + (max (n-5) 0)*precioDescuento5

precioPatatas3 n = 
    if n<=5 
        then n*precioNormal
    else 
        if n<=10 
            then 5*precioNormal + (n-5)*precioDescuento5
            else 5*precioNormal + 5*precioDescuento5 + (n-10)*precioDescuento10

-- ---------------------------------------------------------------------
-- Ejercicio 12. Consideremos el siguiente juego: Dado un número mayor que 1, si es
--  par divídelo entre 2 y si es impar multiplícalo por 3 y súmale 1.
--  Si el resultado es 1 ya has terminado, en caso contrario repite el
--  procedimiento sobre el resultado.

--  Pregunta: Dado un número inicial cualquiera, cuántas veces tendrás
--  que aplicar el procedimiento.

--  Ejemplos:

--  Si empezamos por 10 => dividimos por 2 y obtenemos 5 =>
--  multiplicamos por 3 y sumamos 1, obteniendo 16 => toca volver a
--  dividir y obtenemos 8 => repetimos y obtenemos 4 => seguimos y
--  obtenemos 2 => alcanzamos el 1.

--  los valores han sido 5, 16, 8, 4, 2, 1: lo hemos aplicado 6 veces

--  Si empezamos por 7 los valores serán 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5,
--  16, 8, 4, 2, 1: lo hemos aplicado 16 veces.


--  * Definir una función que aplique una vez el procedimiento
--    anterior. Utilizarla sucesivamente para verificar que los
--    resultados proporcionados a partir de 10 y de 7 son correctos.

--    Nota: Pueden ser de utilidad las funciones even y div

--  * Definir una función que dado un número natural mayor que uno
--    calcule el número de veces que se repite el resultado.

--  * Definir una función que devuelva la lista de resultados hasta
--    llegar a 1.
-- ---------------------------------------------------------------------

pasoJuego x = if (even x)
    then x `div` 2
    else x*3 + 1 
    
contarPasos i n = if (pasoJuego n) == 1
    then i+1
    else contarPasos (i+1) (pasoJuego n)

solPasos n = contarPasos 0 n

genListaPasos xs n = if (pasoJuego n) == 1
    then xs ++ [n,1]
    else genListaPasos (xs ++ [n]) (pasoJuego n)

solLista n = genListaPasos [] n

solucion n = (solPasos n, solLista n)

--alternativa recursiva "mas funcional"

solPasos' 1 = 0 --paso base
solPasos' n = 1 + solPasos' (pasoJuego n)

solLista' 1 = [1] --paso base
solLista' n = n : (solLista'(pasoJuego n))

solucion' n = (solPasos' n, solLista' n)

{- solucion 10 = 
10:(solucion 5) =
10:(5:(solucion 16)) = 
10:(5:(16:(solucion 8)))=
...
10:5:16:8...[1]=
[10,5,16,8,4,2,1] -}