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
sucFG k = suc
  where ks = digitos k
        d = length ks
        suc = ks ++ aux [drop r suc | r <- [0..d-1]]
          where aux xss = sum (map head xss) : aux (map tail xss)
          
digitos :: Integer -> [Integer]
digitos 0 = []
digitos x = digitos (x `div` 10) ++ [x `mod` 10]


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
-- 1ª solución (recursión)
encajadas1 :: Eq a => [a] -> [a] -> Bool
encajadas1 [] ys = False
encajadas1 (x:xs) ys = (x:xs) == take (length (x:xs)) ys || encajadas1 xs ys

-- 2ª solución (plegado)
encajadas3 :: Eq a => [a] -> [a] -> Bool
encajadas3 xs ys = foldr (\n r -> drop (length xs - n) xs == take n ys || r) False [1..length ys]

-- 3ª solución (orden superior)
--encajadas4 :: Eq a => [a] -> [a] -> Bool   
encajadas4 xs ys = any (`isPrefixOf` ys) (init.tails $ xs)


-- 2ª solución
encajadas2 :: Eq a => [a] -> [a] -> Bool
encajadas2 xs ys = aux xs ys [1..length ys]
  where aux xs ys [] = False
        aux xs ys (n:ns) = drop (length xs - n) xs == take n ys || aux xs ys ns


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

prop_encajadas xs ys n = n <= 0 || length xs == 0 || 
                       ((encajadas1 xs ys_encajada == True) &&  
                        (encajadas2 xs ys_encajada == True) && 
                        (encajadas3 xs ys_encajada == True) && 
                        (encajadas1 xs ys == encajadas2 xs ys) &&
                        (encajadas2 xs ys == encajadas3 xs ys))
  where trozo = take n xs
        ys_encajada = trozo ++ ys


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

-- 1) La definición por recursión es
originalDescripcionR :: [Int] -> [Int]
originalDescripcionR [] = []
originalDescripcionR [x] = []
originalDescripcionR (x1:x2:xs) =
    (replicate x1 x2) ++ originalDescripcionR xs

-- 2) La definición por plegado (con 'foldr') es
originalDescripcionP :: [Int] -> [Int]
originalDescripcionP xs =
    foldr (\ (x1,x2) r -> (replicate x1 x2)++r) [] (troceados xs)

troceados :: [Int] -> [(Int,Int)]
troceados [] = []
troceados [x] = []
troceados (x1:x2:xs) =
    (x1,x2) : troceados xs
   
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

-- 1ª definición
carga :: [Int] -> Int
carga xs = length [x | x <- xs, x > 0] - length [x | x <- xs, x < 0]
-- 2ª definición
carga2 :: [Int] -> Int
carga2 xs = sum [signum x | x <- xs]
-- 3ª definición
carga3 :: [Int] -> Int
carga3 [] = 0
carga3 (x:xs) = signum x + carga xs
-- 4ª definición
carga4 :: [Int] -> Int
carga4 = sum . map signum

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
equilibrada xs = abs (carga xs) <= 1



