-- -----------------------------------------------------------------------------
-- Programación Declarativa 2022/23
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Simulación Parcial 1                                  25 de Noviembre de 2022
-- -----------------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- UVUS:
-- Laboratorio/Puesto:
-- -----------------------------------------------------------------------------
-- INSTRUCCIONES PARA LA ENTREGA
-- 1. CAMBIA EL NOMBRE de este archivo por:       <uvus>.hs
--    donde "<uvus>" es tu UVUS.
-- 2. COMENTA LAS LÍNEAS CON ERRORES hasta que se pueda cargar el fichero
--    sin problemas. ESCRIBE tu nombre y apellidos en la cabecera.
-- 3. COMPRIME este archivo en un único fichero llamado EXACTAMENTE:
--      ENTREGA-<uvus>.tar.gz      (o bien)       ENTREGA-<uvus>.tar.xz
--    donde "<uvus>" es tu UVUS. No te olvides del guión después de
--    ENTREGA, y NO lo comprimas en un fichero .zip.
-- 4. REINICIA el equipo. En el menú de selección del sistema (con fondo
--    blanco), HAZ CLICK SOBRE "Enviar examen" al lado de sistema Ubuntu.
-- 5. Pregunta al profesor si ha llegado tu correo correctamente, si es
--    así, ya puedes dejar tu puesto SIN APAGAR EL EQUIPO.
-- ----------------------------------------------------------------------
-- ORIENTACIONES
-- · Escribe la solución de cada ejercicio en el hueco reservado para
--   ello.
-- · Se valorará el uso correcto de tipados para cada función definida.
-- · Puedes añadir tantas funciones auxiliares como necesites (incluyendo su 
--   signatura adecuadamente).
-- · Puedes usar otros módulos de Haskell que estén ya importados.
-- -----------------------------------------------------------------------------

import Data.List
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1 (2.5 puntos) Para cada número n con k dígitos se define una sucesión
-- de tipo Fibonacci cuyos k primeros elementos son los dígitos de n y
-- los siguientes se obtienen sumando los k anteriores términos de la
-- sucesión. Por ejemplo, la sucesión definida por 197 es
-- 1, 9, 7, 17, 33, 57, 107, 197, ...
--
-- Definir la función
-- sucFG :: Integer -> [Integer]
-- tal que (sucFG n) es la sucesión de tipo Fibonacci definida por
-- n. Por ejemplo,
-- take 10 (sucFG 197) == [1,9,7,17,33,57,107,197,361,665]
-- ---------------------------------------------------------------------

sucFG :: Integer -> [Integer]
sucFG n = (digitos n) ++ (sucFGAux n (length (digitos n)) (digitos n))

sucFGAux n k ac = [proxTerm] ++ sucFGAux n k (ac ++ [proxTerm])
  where
    proxTerm = sum (take k (reverse ac))

digitos :: Integer -> [Integer]
digitos x
  | x < 10    = [x]
  | otherwise = (digitos (x `div` 10)) ++ [x `mod` 10]


-- ---------------------------------------------------------------------
-- Ejercicio 2.1 (2 puntos). Decimos que dos listas xs e ys encajan, si hay un trozo
-- no nulo al final de la lista xs que aparece al comienzo de la lista
-- ys. Por ejemplo [1,2,3,4,5,6] y [5,6,7,8] encajan, pues el trozo con
-- los dos últimos elementos de la primera lista, [5,6], aparece al
-- comienzo de la segunda lista.
--
-- Consideramos la función
-- encajadas :: Eq a => [a] -> [a] -> [a]
-- tal que (encajadas xs ys) se verifica si las listas xs e ys encajan.
-- Por ejemplo,
-- encajadas [1,2,3,4,5,6] [5,6,7,8] == True
-- encajadas [4,5,6] [6,7,8] == True
-- encajadas [4,5,6] [4,3,6,8] == False
-- encajadas [4,5,6] [7,8] == False
--
-- Definir por recursión, plegado y orden superior la función 
-- encajadas
--
-- Ayuda: la función tails devuelve la lista de todas las colas que 
-- se pueden estraer de una lista. Por ejemplo tails [1..4] devolvería
-- [[1,2,3,4],[2,3,4],[3,4],[4],[]]
-- La función isPrefixOf determinar si una lista es prefijo de otra. 
-- Por ejemplo isPrefixOf [1..4] [1..10] devolvería True
-- ---------------------------------------------------------------------

encajadasR :: Eq a => [a] -> [a] -> Bool
encajadasR [] _ = False
encajadasR _ [] = False
encajadasR xs ys = (isPrefixOf xs ys) || (encajadasR (tail xs) ys)

encajadasP :: Eq a => [a] -> [a] -> Bool
encajadasP [] _ = False
encajadasP _ [] = False
encajadasP xs ys = foldr (\x r -> (isPrefixOf x ys) || r) False colas
  where colas = reverse (tail (reverse (tails xs)))

encajadasO :: Eq a => [a] -> [a] -> Bool
encajadasO [] _ = False
encajadasO _ [] = False
encajadasO xs ys = or (map prefixCheck colas)
  where 
    colas = reverse (tail (reverse (tails xs)))
    prefixCheck xs = isPrefixOf xs ys

-- ---------------------------------------------------------------------
-- Ejercicio 2.2 (0.5 puntos). Definir por recursión una propiedad que 
-- determine que todas las implementaciones anteriores obtienen el 
-- mismo resultado.
-- 
-- Asegurar que en cada prueba siempre se comprueba con dos listas
-- encajadas además de otras dos listas aleatorias.
--
-- Comprobar con quickCheck
-- ---------------------------------------------------------------------

prop_encajadas xs ys = 
  (((encajadasR xs ys) == (encajadasP xs ys)) 
  == ((encajadasP xs ys) == (encajadasO xs ys))) ==
  (((encajadasR xs (tail xs)) == (encajadasP xs (tail xs))) 
  == ((encajadasP xs (tail xs)) == (encajadasO xs (tail xs))))

-- *Main> quickCheck prop_encajadas 
-- +++ OK, passed 100 tests.


-- ----------------------------------------------------------------------------
-- Ejercicio 3. (2.5 ptos) Una lista de números se puede describir indicando
-- cuantas veces se repite cada elemento. Por ejemplo la lista [1,1,1,3,3,2,2]
-- se puede describir indicando que hay 3 unos, 2 treses y 2 doses. De esta
-- forma, la descripción de una lista es otra lista en la que se indica qué
-- elementos hay en la primera y cuántas veces se repiten. Por ejemplo, la
-- descripción de la lista [1,1,1,3,3,2,2] es [3,1,2,3,2,2]. Ocasionalmente, la
-- descripción de una lista es más corta que la propia lista.
--
-- Se considera la función
--   originalDescripcion :: [Int] -> [Int]
-- tal que '(originalDescripcion xs)' es la lista 'ys' tal que la descripción
-- de 'ys' es la lista 'xs'. Es decir, la lista 'xs' indica qué elementos hay
-- en 'ys' y cuántas veces se repiten. Por ejemplo,
--   originalDescripcion [3,1,2,3,2,2]  ==  [1,1,1,3,3,2,2]
--   originalDescripcion [1,1,3,2,2,3]  ==  [1,2,2,2,3,3]
--   originalDescripcion [2,1,3,3,3,1]  ==  [1,1,3,3,3,1,1,1]
--   originalDescripcion []             ==  []
--   originalDescripcion [2]            ==  []
--
-- Definir esta función
-- 1) por recursión y
-- 2) por plegado (con 'foldr').
-- ----------------------------------------------------------------------------

originalDescripcionR :: [Int] -> [Int]
originalDescripcionR [] = []
originalDescripcionR (x:[]) = []
originalDescripcionR (n:x:xs) = (repetir n x) ++ (originalDescripcionR xs)

repetir :: Int -> Int -> [Int]
repetir n x = [x | i <- [1..n]]

originalDescripcionF :: [Int] -> [Int]
originalDescripcionF xs = foldr (\x r -> (uncurry repetir x) ++ r) [] (listaPares xs)

listaPares :: [Int] -> [(Int,Int)]
listaPares [] = []
listaPares (x:[]) = []
listaPares (a:b:xs) = [(a,b)] ++ listaPares xs

-- ---------------------------------------------------------------------
-- Ejercicio 4.1 (1.5 puntos). La carga de una lista es el número de elementos
-- estrictamente positivos menos el número de elementos estrictamente
-- negativos.
--
-- Definir la función
-- carga :: [Int] -> Int
-- tal que (carga xs) es la carga de la lista xs. Por ejemplo,
-- carga [1,0,2,0,3] == 3
-- carga [1,0,-2,0,3] == 1
-- carga [1,0,-2,0,-3] == -1
-- carga [1,0,-2,2,-3] == 0
-- --------------------------------------------------------------------   

carga :: [Int] -> Int
carga xs = sum [signum x | x <- xs]

-- ---------------------------------------------------------------------
-- Ejercicio 4.2 (1 punto) Una lista es equilibrada si el número de elementos
-- estrictamente positivos difiere en, a lo más, una unidad del número
-- de elementos estrictamente negativos.
--
-- Definir la función
-- equilibrada :: [Int] -> Bool
-- tal que (equilibrada xs) se verifica si xs es una lista
-- equilibrada. Por ejemplo,
-- equilibrada [1,0,2,0,3] == False
-- equilibrada [1,0,-2,0,3] == True
-- equilibrada [1,0,-2,0,-3] == True
-- ---------------------------------------------------------------------

equilibrada :: [Int] -> Bool
equilibrada xs = (abs (carga xs)) <= 1



