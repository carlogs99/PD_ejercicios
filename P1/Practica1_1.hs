-- PD-Practica 1.1
-- Definiciones de funciones, tipos y clases.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================
import Data.Char
import Data.List
import Data.Bool

-- Definir previamente los tipos de la funciones a desarrollar.


-- ---------------------------------------------------------------------
-- Ejercicio 1. Define una función que determina si un elemento está
-- presente en una lista.
--
-- *Main> contiene "Hola mundo" 'b'
-- False
-- *Main> contiene "Hola mundo" 'a'
-- True
-- *Main> contiene "Hola mundo" 'm'
-- True
-- *Main> contiene [] 'm'
-- False
-- ---------------------------------------------------------------------

contiene' xs a = if not (quedanElementos xs)
    then False
    else if (esIgual xs a)
        then True
        else contiene' (tail xs) a

    where 
        quedanElementos = not . null
        esIgual xs a = (head xs) == a

--solucion elegante:
contiene [] _ = False
contiene xs a = (head xs == a ) || (contiene (tail xs) a)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Define una función que devuelva la índice de un elemento
-- dentro de una lista.
--
-- *Main> indice 'b' "Hola mundo"
-- -1
-- *Main> indice 'a' "Hola mundo"
-- 3
-- *Main> indice 'u' "Hola mundo"
-- 6
-- ---------------------------------------------------------------------

indice' a xs = if not (contiene xs a)
    then -1
    else buscoElemento xs a 0
    
    where 
        buscoElemento xs a i = 
            if (esIgual xs a)
                then i 
                else buscoElemento (tail xs) a (i+1)

        esIgual xs a = (head xs) == a

--solucion alternativa (requiere import Data.Bool):
indices :: [Int]
indices = [0..]

indicePre :: Char -> [Char] -> [Int] -> Int
indicePre a [] inds = -1
indicePre a xs inds = bool (indicePre a (tail xs) (tail inds)) (head inds) ((head xs) == a)

indice a xs = indicePre a xs indices

-- ---------------------------------------------------------------------
-- Ejercicio 3. Elimina el n-ésimo elemento de una lista
-- ---------------------------------------------------------------------

--asumo que se indexa desde n=0
eliminaN n xs = (take n xs) ++ (drop (n+1) xs)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Define funciones para determinar si un carácter es una 
-- letra mayúscula, es mínuscula o es un número.
--
-- *Main> esMayus 'A'
-- True
-- *Main> esMayus 'z'
-- False
-- *Main> esMinus 'A'
-- False
-- *Main> esMinus 'z'
-- True
-- ---------------------------------------------------------------------

esMayus x = (x >= 'A') && (x <= 'Z')

esMinus x = (x >= 'a') && (x <= 'z')

esNum x = (x >= '0') && (x <= '9')

-- ---------------------------------------------------------------------
-- Ejercicio 5. Define una función para determinar si un carácter es una 
-- letra mayúscula o es mínuscula o es un número
-- ---------------------------------------------------------------------

esLetraNum x = (esMayus x) || (esMinus x) || (esNum x)

-- ---------------------------------------------------------------------
-- Ejercicio 6. Define una función que determine si hay n elementos 
-- consecutivos de una lista que sumen un valor dado.
--
-- sumaConsecutiva 3 5 [1,2,2,4,5,6,7]
-- True
-- ---------------------------------------------------------------------

sumaConsecutiva _ _ [] = False
--sumaConsecutiva n s xs = ((sum(take n xs)) == s) || (sumaConsecutiva n s (tail xs))

--alternativa con guardas y where:
sumaConsecutiva n s xs 
    | (length xs) < n   = False
    | suma == s         = True
    | otherwise         = sumaConsecutiva n s (tail xs)
    where suma = (sum(take n xs))

--alternativa usando patrones (asumiendo que n=3 siempre):
sumaConsecutiva3 _ [] = False
sumaConsecutiva3 _ [x] = False
sumaConsecutiva3 _ [x,y] = False
sumaConsecutiva3 s (x:y:z:xs)
    | (x+y+z) == s      = True
    | otherwise         = sumaConsecutiva3 s (y:z:xs)

-- ---------------------------------------------------------------------
-- Ejercicio 7. Convierte una frase en capital case:
--
-- capital "hola mundo"
-- "Hola Mundo"
-- capital ""
-- ""
-- ---------------------------------------------------------------------

capitalizarPalabra xs = [(toUpper (head xs))] ++ (tail xs)

capital xs = unwords(map capitalizarPalabra (words xs))









