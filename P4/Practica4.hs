-- PD-Práctica 5
-- Definiciones por recursión.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir, por recursión, la función
--    sumaCuadradosImpares :: [Integer] -> Integer
-- tal que (sumaCuadradosImparesR xs) es la suma de los cuadrados de los
-- números impares de la lista xs. Por ejemplo,
--    sumaCuadradosImpares [1,2,3]  ==  10
-- ---------------------------------------------------------------------

sumaCuadradosImpares :: [Integer] -> Integer
sumaCuadradosImpares xs = sumaCuadradosImparesAux xs 0

sumaCuadradosImparesAux [] ac = ac
sumaCuadradosImparesAux (y:xs) ac
    | even y          = sumaCuadradosImparesAux xs ac
    | otherwise       = sumaCuadradosImparesAux xs (ac + y^2)

-- alternativa con evaluacion por colas:
sumaCuadradosImpares' :: [Integer] -> Integer
sumaCuadradosImpares' [] = 0
sumaCuadradosImpares' (y:xs)
    | odd y     = y^2 + sumaCuadradosImpares' xs
    | otherwise = sumaCuadradosImpares' xs

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir, usando recursión, la función
--    entreL :: Integer -> Integer -> [Integer]
-- tal que (entreL m n) es la lista de los números entre m y n. Por
-- ejemplo, 
--    entreL 2 5  ==  [2,3,4,5]
-- ---------------------------------------------------------------------


entreL a b
    | b < a    = []
    | otherwise = [a] ++ entreL (a+1) b 

-- alternativa por recursion final
entreL' :: Integer -> Integer -> [Integer]
entreL' a b = entreLAUX [] a b

entreLAUX ac a b
    | b < a     = ac
    | otherwise = entreLAUX (ac ++ [a]) (a+1) b


-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir, por recursión, la función
--    sumaPositivosRec :: [Int] -> Int
-- tal que (sumaPositivosRec xs) es la suma de los números positivos de
-- xs. Por ejemplo, 
--    sumaPositivosRec [0,1,-3,-2,8,-1,6]  ==  15
-- ---------------------------------------------------------------------

sumaPositivosRec :: [Int] -> Int
sumaPositivosRec [] = 0
sumaPositivosRec (y:xs)
    | y <= 0    = sumaPositivosRec xs
    | otherwise = y + sumaPositivosRec xs

-- ---------------------------------------------------------------------
-- Ejercicio 4. El doble factorial de un número n se define por 
--    n!! = n*(n-2)* ... * 3 * 1, si n es impar
--    n!! = n*(n-2)* ... * 4 * 2, si n es par
--    1!! = 1
--    0!! = 1    
-- Por ejemplo,
--    8!! = 8*6*4*2   = 384
--    9!! = 9*7*5*3*1 = 945
-- Definir, por recursión, la función
--    dobleFactorial :: Integer -> Integer
-- tal que (dobleFactorial n) es el doble factorial de n. Por ejemplo,
--    dobleFactorial 8  ==  384
--    dobleFactorial 9  ==  945
-- ---------------------------------------------------------------------

dobleFactorial :: Integer -> Integer
dobleFactorial 1 = 1
dobleFactorial 2 = 2
dobleFactorial n = n * dobleFactorial (n-2)

-- alternativa con recursion final:
dobleFactorial' :: Integer -> Integer
dobleFactorial' n = dobleFactorialAUX 1 n

dobleFactorialAUX ac 1 = ac * 1
dobleFactorialAUX ac 2 = ac * 2
dobleFactorialAUX ac n = dobleFactorialAUX (ac * n) (n-2)

-- ---------------------------------------------------------------------
-- Ejercicio 5. La distancia de Hamming entre dos listas es el
-- número de posiciones en que los correspondientes elementos son
-- distintos. Por ejemplo, la distancia de Hamming entre "roma" y "loba"
-- es 2 (porque hay 2 posiciones en las que los elementos
-- correspondientes son distintos: la 1 y la 3).
--    
-- Definir la función
--    distancia :: Eq a => [a] -> [a] -> Int
-- tal que (distancia xs ys) es la distancia de Hamming entre xs e
-- ys. Por ejemplo,
--    distancia "romano" "comino"  ==  2
--    distancia "romano" "camino"  ==  3
--    distancia "roma"   "comino"  ==  2
--    distancia "roma"   "camino"  ==  3
--    distancia "romano" "ron"     ==  1
--    distancia "romano" "cama"    ==  2
--    distancia "romano" "rama"    ==  1
-- ---------------------------------------------------------------------

distancia :: Eq a => [a] -> [a] -> Int
distancia [] _ = 0
distancia _ [] = 0
distancia (x:xs) (y:ys) 
    | x == y    = distancia xs ys
    | otherwise = 1 + distancia xs ys

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir por recursión la función 
--    sustituyeImpar :: [Int] -> [Int]
-- tal que (sustituyeImpar xs) es la lista obtenida sustituyendo cada
-- número impar de xs por el siguiente número par. Por ejemplo,
--    sustituyeImpar [2,5,7,4]  ==  [2,6,8,4]
-- --------------------------------------------------------------------- 

sustituyeImpar :: [Int] -> [Int]
sustituyeImpar [] = []
sustituyeImpar (x:xs)
    | odd x     = [x+1] ++ sustituyeImpar xs
    | otherwise = [x] ++ sustituyeImpar xs

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir, por recursión, la función
--    digitosR :: Integer -> [Integer]
-- tal que (digitosR n) es la lista de los dígitos del número n. Por
-- ejemplo, 
--    digitosR 320274  ==  [3,2,0,2,7,4]
-- ---------------------------------------------------------------------

digitosR :: Integer -> [Integer]
digitosR n 
    | n < 10     = [n]
    | otherwise  = digitosR (div n 10) ++ [mod n 10]

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir por recursión la función
--    potencia :: Integer -> Integer -> Integer
-- tal que (potencia x n) es x elevado al número natural n. Por ejemplo,  
--    potencia 2 3  ==  8
-- ---------------------------------------------------------------------

potencia :: Integer -> Integer -> Integer
potencia _ 0 = 1
potencia x n = x * potencia x (n-1)

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir por recursión la función
--    replicate' :: Int -> a -> [a]
-- tal que (replicate' n x) es la lista formado por n copias del
-- elemento x. Por ejemplo,
--    replicate' 3 2  ==  [2,2,2]
-- ---------------------------------------------------------------------
 
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x:(replicate' (n-1) x)

-- ---------------------------------------------------------------------
-- Ejercicio 10. Dados dos números naturales, a y b, es posible
-- calcular su máximo común divisor mediante el Algoritmo de
-- Euclides. Este algoritmo se puede resumir en la siguiente fórmula:
--    mcd(a,b) = a,                   si b = 0
--             = mcd (b, a módulo b), si b > 0
-- 
-- Definir la función 
--    mcd :: Integer -> Integer -> Integer
-- tal que (mcd a b) es el máximo común divisor de a y b calculado
-- mediante el algoritmo de Euclides. Por ejemplo,
--    mcd 30 45  ==  15
-- ---------------------------------------------------------------------

mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (a `mod` b)

-- ---------------------------------------------------------------------
-- Ejercicio 11. En un templo hindú se encuentran tres varillas de
-- platino. En una de ellas, hay 64 anillos de oro de distintos radios,
-- colocados de mayor a menor.
-- 
-- El trabajo de los monjes de ese templo consiste en pasarlos todos a
-- la tercera varilla, usando la segunda como varilla auxiliar, con las
-- siguientes condiciones: 
--   * En cada paso sólo se puede mover un anillo.
--   * Nunca puede haber un anillo de mayor diámetro encima de uno de
--     menor diámetro.
-- La leyenda dice que cuando todos los anillos se encuentren en la
-- tercera varilla, será el fin del mundo.  
-- 
-- Definir la función 
--    numPasosHanoi :: Integer -> Integer
-- tal que (numPasosHanoi n) es el número de pasos necesarios para
-- trasladar n anillos. Por ejemplo, 
--    numPasosHanoi 2   ==  3
--    numPasosHanoi 7   ==  127
--    numPasosHanoi 64  ==  18446744073709551615
-- ---------------------------------------------------------------------

-- Sean A, B y C las tres varillas. La estrategia recursiva es la
-- siguiente: 
-- * Caso base (N=1): Se mueve el disco de A a C. ==> 1 paso
-- * Caso inductivo (N=M+1): Se mueven M discos de A a B, despues 1 disco de A a C 
-- y M discos de B a C ==> 2*numPasosHanoi m + 1
-- ---------------------------------------------------------------------

numPasosHanoi :: Integer -> Integer
numPasosHanoi 1 = 1
numPasosHanoi n = 2*(numPasosHanoi (n-1)) + 1

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir por recursión la función
--    and' :: [Bool] -> Bool
-- tal que (and' xs) se verifica si todos los elementos de xs son
-- verdadero. Por ejemplo,
--    and' [1+2 < 4, 2:[3] == [2,3]]  ==  True
--    and' [1+2 < 3, 2:[3] == [2,3]]  ==  False
-- ---------------------------------------------------------------------

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && (and' xs)

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir por recursión la función
--    elem' :: Eq a => a -> [a] -> Bool
-- tal que (elem' x xs) se verifica si x pertenece a la lista xs. Por
-- ejemplo, 
--    elem' 3 [2,3,5]  ==  True
--    elem' 4 [2,3,5]  ==  False
-- ---------------------------------------------------------------------

elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (y:xs) = (x == y) || elem' x xs

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir por recursión la función
--    last' :: [a] -> a
-- tal que (last' xs) es el último elemento de xs. Por ejemplo,
--    last' [2,3,5]  =>  5
-- ---------------------------------------------------------------------

last' :: [a] -> a
last' (x:xs) 
    | (length xs == 0)  = x
    | otherwise         = last' xs

-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir por recursión la función
--    concat' :: [[a]] -> [a]
-- tal que (concat' xss) es la lista obtenida concatenando las listas de
-- xss. Por ejemplo,
--    concat' [[1..3],[5..7],[8..10]]  ==  [1,2,3,5,6,7,8,9,10]
-- ---------------------------------------------------------------------
 
concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

-- ---------------------------------------------------------------------
-- Ejercicio 16. Definir por recursión la función
--    selecciona :: [a] -> Int -> a
-- tal que (selecciona xs n) es el n-ésimo elemento de xs. Por ejemplo,
--    selecciona [2,3,5,7] 2  ==  5 
-- ---------------------------------------------------------------------

selecciona :: [a] -> Int -> a
selecciona (x:_) 0 = x
selecciona (x:xs) n = selecciona xs (n-1)

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir por recursión la función
--    take' :: Int -> [a] -> [a]
-- tal que (take' n xs) es la lista de los n primeros elementos de
-- xs. Por ejemplo, 
--    take' 3 [4..12]  =>  [4,5,6]
-- ---------------------------------------------------------------------

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x:(take' (n-1) xs)

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir por recursión la función
--    mezcla :: Ord a => [a] -> [a] -> [a] 
-- tal que (mezcla xs ys) es la lista obtenida mezclando las listas
-- ordenadas xs e ys. Por ejemplo,  
--    mezcla [2,5,6] [1,3,4]  ==  [1,2,3,4,5,6]
-- ---------------------------------------------------------------------

mezcla :: Ord a => [a] -> [a] -> [a] 
mezcla xs [] = xs
mezcla [] ys = ys
mezcla (x:xs) (y:ys) 
    | y < x     = y:(mezcla (x:xs) ys)
    | otherwise = x:(mezcla xs (y:ys))

-- ---------------------------------------------------------------------
-- Ejercicio 19. Definir la función 
--    mitades :: [a] -> ([a],[a]) 
-- tal que (mitades xs) es el par formado por las dos mitades en que se
-- divide xs tales que sus longitudes difieren como máximo en uno. Por
-- ejemplo, 
--    mitades [2,3,5,7,9]  ==  ([2,3],[5,7,9])
-- ---------------------------------------------------------------------

mitades :: [a] -> ([a],[a])
mitades xs = (take m xs, drop m xs)
    where m = div (length xs) 2

-- ---------------------------------------------------------------------
-- Ejercicio 20. Definir por recursión la función 
--    ordMezcla :: Ord a => [a] -> [a]
-- tal que (ordMezcla xs) es la lista obtenida ordenado xs por mezcla
-- (es decir, considerando que la lista vacía y las listas unitarias
-- están ordenadas y cualquier otra lista se ordena mezclando las dos
-- listas que resultan de ordenar sus dos mitades por separado). Por
-- ejemplo, 
--    ordMezcla [5,2,3,1,7,2,5]  ==  [1,2,2,3,5,5,7]
-- ---------------------------------------------------------------------

ordMezcla :: Ord a => [a] -> [a]
ordMezcla [] = []
ordMezcla (x:xs)
    | (length xs == 0)  = [x]
    | otherwise         = mezcla (quicksort (fst (mitades (x:xs)))) (quicksort (snd (mitades (x:xs))))

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = (quicksort menores) ++ [p] ++ (quicksort mayores)
    where 
        menores = [x | x <- xs, x <= p]
        mayores = [x | x <- xs, x > p]
    
-- ---------------------------------------------------------------------
-- Ejercicio 21. Definir por recursión la función
--    borra :: Eq a => a -> [a] -> [a]
-- tal que (borra x xs) es la lista obtenida borrando una ocurrencia de
-- x en la lista xs. Por ejemplo, 
--    borra 1 [1,2,1]  ==  [2,1]
--    borra 3 [1,2,1]  ==  [1,2,1]
-- ---------------------------------------------------------------------

borra :: Eq a => a -> [a] -> [a]
borra _ [] = []
borra a (x:xs)
    | (x == a)  = xs ++ borra a []
    | otherwise = x:(borra a xs)