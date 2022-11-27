-- PD - 2021/22
-- Correspondiente a Relación 21 de I1M 2010-20
-- El TAD de las pilas.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación de ejercicios es definir funciones sobre 
-- el TAD de las pilas.

-- Para realizar los ejercicios hay descargar las implementaciones
-- de las pilas:
-- + PilaTA.hs que está en https://www.cs.us.es/cursos/pd-2021/ejercicios/PilaTA.hs
-- + PilasL.hs que está en https://www.cs.us.es/cursos/pd-2021/ejercicios/PilaL.hs

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

import Data.List
import Test.QuickCheck

-- Hay que elegir una implementación del TAD pilas.
-- import PilaTA
-- import PilaL
import PilaTA

-- ---------------------------------------------------------------------
-- A lo largo de la relación de ejercicios usaremos los siguientes
-- ejemplos de pilas:
-- ---------------------------------------------------------------------

ejP1, ejP2, ejP3, ejP4, ejP5 :: Pila Int
ejP1 = foldr apila vacia [1..20]
ejP2 = foldr apila vacia [2,5..18]
ejP3 = foldr apila vacia [3..10]
ejP4 = foldr apila vacia [4,-1,7,3,8,10,0,3,3,4]
ejP5 = foldr apila vacia [1..5]

-- ---------------------------------------------------------------------
-- Ejercicio 1: Definir la función
--    filtraPila :: (a -> Bool) -> Pila a -> Pila a
-- tal que (filtraPila p pila) es la pila con los elementos de pila
-- que verifican el predicado p, en el mismo orden. Por ejemplo,
--    ghci> ejP1
--    1|2|3|4|5|6|7|8|9|10|11|12|13|14|15|16|17|18|19|20|-
--    ghci> filtraPila even ejP1
--    2|4|6|8|10|12|14|16|18|20|-

-- ---------------------------------------------------------------------

filtraPila :: (a -> Bool) -> Pila a -> Pila a
filtraPila f p
    | esVacia p     = vacia
    | f (cima p)    = apila (cima p) (filtraPila f (desapila p))
    | otherwise     = filtraPila f (desapila p) 

-- ---------------------------------------------------------------------
-- Ejercicio 2: Definir la función
--    mapPila :: (a -> a) -> Pila a -> Pila a
-- tal que (mapPila f pila) es la pila formada con las imágenes por f de
-- los elementos de pila, en el mismo orden. Por ejemplo,
--    ghci> mapPila (+7) ejP1
--    8|9|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|-
-- ---------------------------------------------------------------------

mapPila :: (a -> a) -> Pila a -> Pila a
mapPila f p
    | esVacia p     = vacia
    | otherwise     = apila (f (cima p)) (mapPila f (desapila p))

-- ---------------------------------------------------------------------
-- Ejercicio 3: Definir la función
--    pertenecePila :: (Eq a) => a -> Pila a -> Bool
-- tal que (pertenecePila y p) se verifica si y sólo si y es un elemento
-- de la pila p. Por ejemplo,
--    pertenecePila 7 ejP1  == True
--    pertenecePila 70 ejP1 == False
-- ---------------------------------------------------------------------

pertenecePila :: (Eq a) => a -> Pila a -> Bool
pertenecePila a p = 
    if esVacia p 
        then False 
    else 
        (c == a) || (pertenecePila a dp)
    where
        c = cima p
        dp = desapila p

-- ---------------------------------------------------------------------
-- Ejercicio 4: definir la función
--    contenidaPila :: (Eq a) => Pila a -> Pila a -> Bool
-- tal que (contenidaPila p1 p2) se verifica si y sólo si todos los
-- elementos de p1 son elementos de p2. Por ejemplo,
--    contenidaPila ejP2 ejP1 == True
--    contenidaPila ejP1 ejP2 == False
-- ---------------------------------------------------------------------

contenidaPila :: (Eq a) => Pila a -> Pila a -> Bool
contenidaPila p1 p2 = 
    if esVacia p1
        then True
    else
        (pertenecePila c1 p2) && (contenidaPila dp1 p2)
    where
        c1 = cima p1
        dp1 = desapila p1


-- ---------------------------------------------------------------------
-- Ejercicio 4: Definir la función
--    prefijoPila :: (Eq a) => Pila a -> Pila a -> Bool
-- tal que (prefijoPila p1 p2) se verifica si la pila p1 es justamente
-- un prefijo de la pila p2. Por ejemplo,
--    prefijoPila ejP3 ejP2 == False
--    prefijoPila ejP5 ejP1 == True
-- ---------------------------------------------------------------------

prefijoPila :: (Eq a) => Pila a -> Pila a -> Bool
prefijoPila p1 p2 
    | esVacia p1    = True
    | esVacia p2    = False
    | otherwise     = (c1 == c2) && (prefijoPila dp1 dp2)
    where
        c1 = cima p1
        c2 = cima p2
        dp1 = desapila p1
        dp2 = desapila p2

-- ---------------------------------------------------------------------
-- Ejercicio 5: Definir la función
--    subPila :: (Eq a) => Pila a -> Pila a -> Bool
-- tal que (subPila p1 p2) se verifica si p1 es una subpila de p2.
-- Por ejemplo, 
--    subPila ejP2 ejP1 == False
--    subPila ejP3 ejP1 == True
-- ---------------------------------------------------------------------

subPila :: (Eq a) => Pila a -> Pila a -> Bool
subPila p1 p2 
    | esVacia p2    = False
    | otherwise     = (prefijoPila p1 p2) || (subPila p1 dp2) 
    where
        dp2 = desapila p2

-- ---------------------------------------------------------------------
-- Ejercicio 6: Definir la función
--    ordenadaPila :: (Ord a) => Pila a -> Bool
-- tal que (ordenadaPila p) se verifica si los elementos de la pila p
-- están ordenados en orden creciente. Por ejemplo,
--    ordenadaPila ejP1 == True
--    ordenadaPila ejP4 == False
-- ---------------------------------------------------------------------

ordenadaPila :: (Ord a) => Pila a -> Bool
ordenadaPila p 
    | esVacia p     = True
    | esVacia dp    = True
    | otherwise     = (primero <= segundo) && (ordenadaPila dp)
    where
        primero = cima p
        dp = desapila p
        segundo = cima dp
        

-- ---------------------------------------------------------------------
-- Ejercicio 7.1: Definir una función
--    lista2Pila :: [a] -> Pila a
-- tal que (lista2Pila xs) es una pila formada por los elementos de xs.
-- Por ejemplo,
--    lista2Pila [1..6] == 1|2|3|4|5|6|-
-- ---------------------------------------------------------------------

lista2Pila :: [a] -> Pila a
lista2Pila xs = foldr apila vacia xs 

lista2Pila' :: [a] -> Pila a
lista2Pila' [] = vacia 
lista2Pila' (x:xs) = apila x (lista2Pila' xs)


-- ---------------------------------------------------------------------
-- Ejercicio 7.2: Definir una función
--  pila2Lista :: Pila a -> [a]
-- tal que (pila2Lista p) es la lista formada por los elementos de p.
-- Por ejemplo,
--    pila2Lista ejP2 == [2,5,8,11,14,17]
-- ---------------------------------------------------------------------

pila2Lista :: Pila a -> [a]
pila2Lista p
    | esVacia p     = []
    | otherwise     = c:(pila2Lista dp)
    where
        c = cima p
        dp = desapila p

contenidaPila' :: (Eq a) => Pila a -> Pila a -> Bool
contenidaPila' p1 p2 = and [pertenecePila a p2 | a <- p1s]
    where 
        p1s = pila2Lista p1

-- ---------------------------------------------------------------------
-- Ejercicio 7.3: Comprobar con QuickCheck que la función pila2Lista es
-- la inversa de  lista2Pila, y recíprocamente.
-- ---------------------------------------------------------------------

prop_pila2Lista :: Pila Int -> Bool
prop_pila2Lista p = (lista2Pila (pila2Lista p)) == p

-- ghci> quickCheck prop_pila2Lista
-- +++ OK, passed 100 tests.

prop_lista2Pila :: [Int] -> Bool
prop_lista2Pila xs = (pila2Lista (lista2Pila xs)) == xs

-- ghci> quickCheck prop_lista2Pila
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 9.1: Definir la función 
--    ordenaInserPila :: (Ord a) => Pila a -> Pila a
-- tal que (ordenaInserPila p) es una pila con los elementos de la pila
-- p, ordenados por inserción. Por ejemplo,
--    ejP4 = 4|-1|7|3|8|10|0|3|3|4|-
--    ghci> ordenaInserPila ejP4
--    -1|0|3|3|3|4|4|7|8|10|-
-- ---------------------------------------------------------------------

inserElem :: (Ord a) => a -> Pila a -> Pila a
inserElem n p
    | esVacia p     = apila n vacia
    | c < n        = apila c (inserElem n dp)
    | otherwise     = apila n p
    where
        c = cima p
        dp = desapila p 

ordenaInserPila :: (Ord a) => Pila a -> Pila a
ordenaInserPila p 
    | esVacia p     = vacia
    | otherwise     = inserElem c (ordenaInserPila dp)
    where
        c = cima p
        dp = desapila p 


-- ---------------------------------------------------------------------
-- Ejercicio 9.2: Comprobar con QuickCheck que la pila 
---    (ordenaInserPila p) 
-- está ordenada correctamente.

prop_ordenaInserPila :: Pila Int -> Bool
prop_ordenaInserPila p = ordenadaPila (ordenaInserPila p)

-- ghci> quickCheck prop_ordenaInserPila
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 10.1: Definir la función
--    nubPila :: (Eq a) => Pila a -> Pila a
-- tal que (nubPila p) es una pila con los elementos de p sin
-- repeticiones. Por ejemplo,
--    ghci> ejP4
--    4|-1|7|3|8|10|0|3|3|4|-
--    ghci> nubPila ejP4
--    -1|7|8|10|0|3|4|-
-- ---------------------------------------------------------------------

nubPila :: (Eq a) => Pila a -> Pila a
nubPila p 
    | esVacia p || esVacia dp   = vacia
    | pertenecePila c dp        = nubPila dp
    | otherwise                 = apila c (nubPila dp)
    where
        c = cima p
        dp = desapila p 

reversePila :: Pila a -> Pila a
reversePila p = reversePilaAux p vacia
    where
        reversePilaAux p ac
            | esVacia p = ac
            | otherwise = reversePilaAux dp (apila c ac)
            where
                c = cima p
                dp = desapila p

nubPilaAc :: (Eq a) => Pila a -> Pila a
nubPilaAc p = nubPilaAcAux p vacia
    where
        nubPilaAcAux p ac
            | esVacia p                 = reversePila ac
            | pertenecePila c ac        = nubPilaAcAux dp ac
            | otherwise                 = nubPilaAcAux dp (apila c ac)
            where
                c = cima p
                dp = desapila p


-- ---------------------------------------------------------------------
-- Ejercicio 10.2: Definir la propiedad siguiente: "las composición de
-- las funciones nub y pila2Lista coincide con la composición de las
-- funciones pila2Lista y nubPila", y comprobarla con quickCheck.
-- En caso de ser falsa, redefinir la función nubPila para que se
-- verifique la propiedad.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_nubPila :: Pila Int -> Bool
prop_nubPila p = (nub (pila2Lista p)) == (pila2Lista (nubPilaAc p))

-- La comprobación es falsa inicialmente porque nub evalua de derecha 
-- a izquierda mientras que nubPila lo hace de izquierda a derecha


-- ---------------------------------------------------------------------
-- Ejercicio 11: Definir la función 
--    maxPila :: (Ord a) => Pila a -> a
-- tal que (maxPila p) sea el mayor de los elementos de la pila p. Por
-- ejemplo, 
--    ghci> ejP4
--    4|-1|7|3|8|10|0|3|3|4|-
--    ghci> maxPila ejP4
--    10
-- ---------------------------------------------------------------------

maxPila :: (Ord a) => Pila a -> a
maxPila p = cima (reversePila (ordenaInserPila p))

-- ---------------------------------------------------------------------
-- Generador de pilas                                                 --
-- ---------------------------------------------------------------------

-- genPila es un generador de pilas. Por ejemplo,
--    ghci> sample genPila
--    -
--    0|0|-
--    -
--    -6|4|-3|3|0|-
--    -
--    9|5|-1|-3|0|-8|-5|-7|2|-
--    -3|-10|-3|-12|11|6|1|-2|0|-12|-6|-
--    2|-14|-5|2|-
--    5|9|-
--    -1|-14|5|-
--    6|13|0|17|-12|-7|-8|-19|-14|-5|10|14|3|-18|2|-14|-11|-6|-
genPila :: (Arbitrary a, Num a) => Gen (Pila a)
genPila = do xs <- listOf arbitrary
             return (foldr apila vacia xs)
         


-- El tipo pila es una instancia del arbitrario. 
instance (Arbitrary a, Num a) => Arbitrary (Pila a) where
    arbitrary = genPila


