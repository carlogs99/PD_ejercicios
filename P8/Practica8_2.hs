-- PD 2020-21. Vectores y matrices: ejercicios de exámenes.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación se presenta una recopilación de ejercicios vectores
-- y matrices propuestos en exámenes de la asignatura I1M.

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Array
import Data.List

-- Nota. En la relación usaremos los tipos de los vectores y las matrices 
-- definidos por 

type Vector a = Array Int a
type Matriz a = Array (Int,Int) a

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    esTriangularS :: Num a => Matriz a -> Bool
-- tal que (esTriangularS p) se verifica si p es una matriz triangular
-- superior. Por ejemplo, 
--    ghci> esTriangularS (listArray ((1,1),(3,3)) [1,2,1,0,4,7,0,0,5])
--    True
--    ghci> esTriangularS (listArray ((1,1),(3,3)) [1,2,3,1,2,4,1,2,5])
--    False
-- ---------------------------------------------------------------------

esTriangularS:: (Num a, Eq a) => Matriz a -> Bool
esTriangularS m = and [m!(i,j) == 0 | i <- [1..f], j <- [1..c], i > j]
    where
        (f,c) = snd (bounds m)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    antidiagonal :: (Num a, Eq a) => Matriz a -> Bool
-- tal que (antidiagonal m) se verifica si es cuadrada y todos los
-- elementos de m que no están en su diagonal secundaria son nulos. Por
-- ejemplo,   
--    ghci> antidiagonal (listArray ((1,1),(3,3)) [0,0,4, 0,6,0, 0,0,0])
--    True
--    ghci> antidiagonal (listArray ((1,1),(3,3)) [7,0,4, 0,6,0, 0,0,5])
--    False
-- ---------------------------------------------------------------------

-- m1, m2 :: Matriz Int
-- m1 = listArray ((1,1),(3,3)) [0,0,4, 0,6,0, 0,0,0]
-- m2 = listArray ((1,1),(3,3)) [7,0,4, 0,6,0, 0,0,5]

antidiagonal :: (Num a, Eq a) => Matriz a -> Bool 
antidiagonal m
    | f /= c        = False
    | otherwise     = and [m!(i,j) == 0 | i <- [1..f], j <- [1..c], i+j /= f+1]
    where
        (f,c) = snd (bounds m)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    esEscalar:: Num a => Matriz a -> Bool
-- tal que (esEscalar p) se verifica si p es una matriz es escalar; es
-- decir, diagonal con todos los elementos de la diagonal principal
-- iguales. Por ejemplo,
--    esEscalar (listArray ((1,1),(3,3)) [5,0,0,0,5,0,0,0,5])  ==  True
--    esEscalar (listArray ((1,1),(3,3)) [5,0,0,1,5,0,0,0,5])  ==  False
--    esEscalar (listArray ((1,1),(3,3)) [5,0,0,0,6,0,0,0,5])  ==  False
-- ---------------------------------------------------------------------

esEscalar:: (Num a, Eq a) => Matriz a -> Bool
esEscalar m
    | f /= c || not (esDiagonal m)  = False
    | otherwise                     = length (nub vecDiagon) == 1
    where
        (f,c) = snd (bounds m)
        esDiagonal m = and [m!(i,j) == 0 | i <- [1..f], j <- [1..c], i /= j]
        vecDiagon = [m!(i,j) | i <- [1..f], j <- [1..c], i == j]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función 
--    aplicaT :: (Ix a, Num b) => Array a b -> (b -> c) -> Array a c
-- tal que (aplicaT t f) es la tabla obtenida aplicado la función f a
-- los elementos de la tabla t. Por ejemplo,
--    ghci> aplicaT (array (1,5) [(1,6),(2,3),(3,-1),(4,9),(5,20)]) (+1)
--    array (1,5) [(1,7),(2,4),(3,0),(4,10),(5,21)]
--    ghci> :{
--    *Main| aplicaT (array ((1,1),(2,3)) [((1,1),3),((1,2),-1),((1,3),0),((2,1),0),((2,2),0),((2,3),-1)]) (*2)
--    *Main| :}
--    array ((1,1),(2,3)) [((1,1),6),((1,2),-2),((1,3),0),
--                         ((2,1),0),((2,2),0),((2,3),-2)]
-- ---------------------------------------------------------------------

aplicaT :: (Ix a, Num b) => Array a b -> (b -> c) -> Array a c
aplicaT t f = listArray (bounds t) [f x | x <- (elems t)]


-- ---------------------------------------------------------------------
-- Ejercicio 5. Dada una matriz numérica A de dimensiones (m,n) y una
-- matriz booleana B de las mismas dimensiones, y dos funciones f y g,
-- la transformada de A respecto de B, f y g es la matriz C (de las
-- mismas dimensiones), tal que, para cada celda (i,j):   
--    C(i,j) = f(A(i,j)) si B(i,j) es verdadero
--    C(i,j) = g(A(i,j)) si B(i,j) es falso
-- Por ejemplo, si A y B son las matrices
--    |1 2|   |True  False|  
--    |3 4|   |False True |
-- respectivamente, y f y g son dos funciones tales que f(x) = x+1 y
-- g(x) = 2*x, entonces la transformada de A respecto de B, f y g es
--    |2 4|
--    |6 5|
--     
-- Definir la función
--    transformada :: Matriz a -> Matriz Bool -> (a -> b) -> (a -> b) -> Matriz b
-- tal que (transformada a b f g) es la transformada de A respecto de B,
-- f y g. Por ejemplo,
--  ghci> let a = listArray ((1,1),(2,2)) [1,2,3,4] :: Matriz Int
--  ghci> let b = listArray ((1,1),(2,2)) [True,False,False,True] :: Matriz Bool
--  ghci> transformada a b (+1) (*2)
--  array ((1,1),(2,2)) [((1,1),2),((1,2),4),((2,1),6),((2,2),5)]
-- ---------------------------------------------------------------------

transformada :: Matriz a -> Matriz Bool -> (a -> b) -> (a -> b) -> Matriz b
transformada a b f g = array (bounds a) [((i,j), t i j) | i <- [1..n], j <- [1..m]]
    where
        (n,m) = snd (bounds a)
        t i j = if (b!(i,j)) then (f (a!(i,j))) else (g (a!(i,j)))

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Un vector se denomina estocástico si todos sus
-- elementos son mayores o iguales que 0 y suman 1.  
-- 
-- Definir la función 
--    vectorEstocastico :: Vector Float -> Bool
-- tal que (vectorEstocastico v) se verifica si v es estocástico. Por
-- ejemplo,  
--    vectorEstocastico (listArray (1,5) [0.1, 0.2, 0, 0, 0.7]) == True
--    vectorEstocastico (listArray (1,5) [0.1, 0.2, 0, 0, 0.9]) == False
-- ---------------------------------------------------------------------

vectorEstocastico :: Vector Float -> Bool
vectorEstocastico v = todosPositivos && sumanUno
    where
        todosPositivos = and [x >= 0 | x <- xs]
        sumanUno = sum [x | x <- xs] == 1
        xs = elems v

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Una matriz se denomina estocástica si sus columnas
-- son vectores estocásticos.  
-- 
-- Definir la función 
--    matrizEstocastica :: Matriz Float -> Bool
-- tal que (matrizEstocastico p) se verifica si p es estocástica. Por
-- ejemplo,  
--    matrizEstocastica (listArray ((1,1),(2,2)) [0.1,0.2,0.9,0.8]) == True
--    matrizEstocastica (listArray ((1,1),(2,2)) [0.1,0.2,0.3,0.8]) == False
-- ---------------------------------------------------------------------

matrizEstocastica :: Matriz Float -> Bool        
matrizEstocastica p = and [vectorEstocastico v | v <- cols]
    where
        (m,n) = snd (bounds p)
        cols = [listArray (1,m) [p!(i,j) | i <- [1..m]] | j <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función 
--    maximos :: Matriz Int -> [Int]
-- tal que (maximos p) es la lista de los máximos locales de la matriz
-- p; es decir de los elementos de p que son mayores que todos sus
-- vecinos. Por ejemplo,  
--    ghci> maximos (listArray ((1,1),(3,4)) [9,4,6,5,8,1,7,3,0,2,5,4])
--    [9,7]
-- ya que los máximos locales de la matriz
--    |9 4 6 5|
--    |8 1 7 3|
--    |0 2 5 4|
-- son 9 y 7.
-- ---------------------------------------------------------------------

maximos :: Matriz Int -> [Int]
maximos p = [p!(i,j) | i <- [1..m], j <- [1..n], esMaxLocal p i j]
    where
        (m,n) = snd (bounds p)
        esMaxLocal p i j
                    | (i,j) == (1,1)    = actual > maximum [elemDerecha i j, elemAbajo i j]
                    | (i,j) == (m,n)    = actual > maximum [elemIzquierda i j, elemArriba i j]
                    | (i,j) == (1,n)    = actual > maximum [elemIzquierda i j, elemAbajo i j]
                    | (i,j) == (m,1)    = actual > maximum [elemArriba i j, elemDerecha i j]
                    | i == 1            = actual > maximum [elemIzquierda i j, elemAbajo i j, elemDerecha i j]
                    | j == 1            = actual > maximum [elemArriba i j, elemDerecha i j, elemAbajo i j]
                    | i == m            = actual > maximum [elemArriba i j, elemDerecha i j, elemIzquierda i j]
                    | j == n            = actual > maximum [elemArriba i j, elemAbajo i j, elemIzquierda i j]
                    | otherwise         = actual > maximum [elemAbajo i j, elemArriba i j, elemDerecha i j, elemIzquierda i j]
                    where
                        (m,n) = snd (bounds p)
                        actual = p!(i,j)
                        elemDerecha i j = p!(i,j+1)
                        elemIzquierda i j = p!(i,j-1)
                        elemArriba i j = p!(i-1,j)
                        elemAbajo i j = p!(i+1,j)

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función 
--    algunMenor :: Matriz Int -> [Int]
-- tal que (algunMenor p) es la lista de los elementos de p que tienen
-- algún vecino menor que él. Por ejemplo,  
--    algunMenor (listArray ((1,1),(3,4)) [9,4,6,5,8,1,7,3,4,2,5,4])
--    [9,4,6,5,8,7,4,2,5,4]          
-- pues sólo el 1 y el 3 no tienen ningún vecino menor en la matriz
--    |9 4 6 5|
--    |8 1 7 3|
--    |4 2 5 4|
-- ---------------------------------------------------------------------        

algunMenor :: Matriz Int -> [Int]
algunMenor p = [p!(i,j) | i <- [1..m], j <- [1..n], noEsMinLocal p i j]
    where
        (m,n) = snd (bounds p)
        noEsMinLocal p i j
                    | (i,j) == (1,1)    = actual > minimum [elemDerecha i j, elemAbajo i j]
                    | (i,j) == (m,n)    = actual > minimum [elemIzquierda i j, elemArriba i j]
                    | (i,j) == (1,n)    = actual > minimum [elemIzquierda i j, elemAbajo i j]
                    | (i,j) == (m,1)    = actual > minimum [elemArriba i j, elemDerecha i j]
                    | i == 1            = actual > minimum [elemIzquierda i j, elemAbajo i j, elemDerecha i j]
                    | j == 1            = actual > minimum [elemArriba i j, elemDerecha i j, elemAbajo i j]
                    | i == m            = actual > minimum [elemArriba i j, elemDerecha i j, elemIzquierda i j]
                    | j == n            = actual > minimum [elemArriba i j, elemAbajo i j, elemIzquierda i j]
                    | otherwise         = actual > minimum [elemAbajo i j, elemArriba i j, elemDerecha i j, elemIzquierda i j]
                    where
                        (m,n) = snd (bounds p)
                        actual = p!(i,j)
                        elemDerecha i j = p!(i,j+1)
                        elemIzquierda i j = p!(i,j-1)
                        elemArriba i j = p!(i-1,j)
                        elemAbajo i j = p!(i+1,j)

-- ---------------------------------------------------------------------
-- Ejercicio 9.1. Definir la función
--      proporcional :: (Fractional a, Eq a) => Vector a -> Vector a -> Bool
-- tal que (proporcional v1 v2) verifica si los vectores v1 y v2 son proporcionales, 
-- es decir, v1 == k*v2, con un k un número escalar cualquiera. Por ejemplo,
--    ghci> let p1 = listArray ((1,1),(3,3)) [1,0,0,0,0,1,0,1,0] :: Matriz Float
--    ghci> let v1 = listArray (1,3) [0,-1,1] :: Vector Float
--    ghci> let v2 = listArray (1,3) [1,2,1] :: Vector Float
--    proporcional v1 v1                           = True
--    proporcional v1 v2                           = False
--    proporcional v1 (listArray (1,3) [0,-5,5])   = True
--    proporcional v1 (listArray (1,3) [0,-5,4])   = False
--    proporcional (listArray (1,3) [0,-5,5]) v1   = True
--    proporcional v1 (listArray (1,3) [0,0,0])    = True
--    proporcional (listArray (1,3) [0,0,0]) v1    = False

proporcional :: (Fractional a, Eq a) => Vector a -> Vector a -> Bool
proporcional v1 v2 = length (nub listaK) == 1
    where 
        listaK = [k (v1!i) (v2!i) | i <- [1..n], (k (v1!i) (v2!i)) /= Nothing]
        n = snd (bounds v1)
        k x y
            | x == 0            = Nothing
            | y == 0            = Just 0
            | otherwise         = Just (x/y)

-- ---------------------------------------------------------------------
-- Ejercicio 9.2. Definir la función
--    esAutovector :: (Fractional a, Eq a) => 
--                    Vector a -> Matriz a -> Bool
-- tal que (esAutovector v p) compruebe si v es un autovector de p
-- (es decir, el producto de v por p es un vector proporcional a
-- v). Por ejemplo, 
--    ghci> let p1 = listArray ((1,1),(3,3)) [1,0,0,0,0,1,0,1,0] :: Matriz Float
--    ghci> let v1 = listArray (1,3) [0,-1,1] :: Vector Float
--    ghci> let v2 = listArray (1,3) [1,2,1] :: Vector Float
--    ghci> esAutovector v1 p1 
--    True
--    ghci> esAutovector v2 p1 
--    False
-- ---------------------------------------------------------------------

esAutovector :: (Fractional a, Eq a) => Vector a -> Matriz a -> Bool
esAutovector v p = proporcional v (producto v p)
    where
        (m,n) = snd (bounds p)
        producto v p
            | snd (bounds v) /= m   = error "Dimension incorrecta"
            | otherwise             = listArray (1,n) [sum [(v!i)*(p!(i,j)) | i <- [1..m]] | j <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 9.3. Definir la función
--    autovalorAsociado :: (Fractional a, Eq a) => 
--                         Matriz a -> Vector a -> Maybe a
-- tal que si v es un autovector de p, calcule el autovalor asociado.
-- Por ejemplo,
--    ghci> let p1 = listArray ((1,1),(3,3)) [1,0,0,0,0,1,0,1,0] :: Matriz Float
--    ghci> let v1 = listArray (1,3) [0,-1,1] :: Vector Float
--    ghci> let v2 = listArray (1,3) [1,2,1] :: Vector Float
--    autovalorAsociado p1 v1 == Just (-1.0)
--    autovalorAsociado p1 v2 == Nothing
-- ---------------------------------------------------------------------

autovalorAsociado :: (Fractional a, Eq a) => 
                     Matriz a -> Vector a -> Maybe a
autovalorAsociado p v
    | esAutovector v p  = calcularAutovalor v p
    | otherwise         = Nothing
    where
        (m,n) = snd (bounds p)
        producto v p
            | snd (bounds v) /= m   = error "Dimension incorrecta"
            | otherwise             = listArray (1,n) [sum [(v!i)*(p!(i,j)) | i <- [1..m]] | j <- [1..n]]
        k x y
            | x == 0            = Nothing
            | y == 0            = Just 0
            | otherwise         = Just (x/y)
        listaK v1 v2 = [k (v1!i) (v2!i) | i <- [1..n], (k (v1!i) (v2!i)) /= Nothing]
        calcularAutovalor v p = head (listaK v (producto v p))

-- ------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    sumaVecinos :: Matriz Int -> Matriz Int
-- tal que (sumaVecinos p) es la matriz obtenida al escribir en la 
-- posicion (i,j) la suma de los todos vecinos del elemento que ocupa 
-- el lugar (i,j) en la matriz p. Por ejemplo,
--    ghci> sumaVecinos (listArray ((1,1),(3,3)) [0,1,3, 1,2,0, 0,5,7])
--    array ((1,1),(3,3)) [((1,1),4),((1,2), 6),((1,3), 3),
--                         ((2,1),8),((2,2),17),((2,3),18),
--                         ((3,1),8),((3,2),10),((3,3), 7)]
-- ------------------------------------------------------------------

sumaVecinos :: Matriz Int -> Matriz Int
sumaVecinos p = listArray (bounds p) [sum (vecinos p i j) | i <- [1..m], j <- [1..n]]
    where
        (m,n) = snd (bounds p)
        vecinos p i j
            | (i,j) == (1,1)    = [elemDerecha i j, elemAbajo i j, elemAbDer i j]
            | (i,j) == (m,n)    = [elemIzquierda i j, elemArriba i j, elemArIzq i j]
            | (i,j) == (1,n)    = [elemIzquierda i j, elemAbajo i j, elemAbIzq i j]
            | (i,j) == (m,1)    = [elemArriba i j, elemDerecha i j, elemArDer i j]
            | i == 1            = [elemIzquierda i j, elemAbajo i j, elemDerecha i j, elemAbDer i j, elemAbIzq i j]
            | j == 1            = [elemArriba i j, elemDerecha i j, elemAbajo i j, elemAbDer i j, elemArDer i j]
            | i == m            = [elemArriba i j, elemDerecha i j, elemIzquierda i j, elemArDer i j, elemArIzq i j]
            | j == n            = [elemArriba i j, elemAbajo i j, elemIzquierda i j, elemAbIzq i j, elemArIzq i j]
            | otherwise         = [elemAbajo i j, elemArriba i j, elemDerecha i j, elemIzquierda i j, elemAbDer i j, elemAbIzq i j, elemArIzq i j, elemArDer i j]
            where
                (m,n) = snd (bounds p)
                actual = p!(i,j)
                elemDerecha i j = p!(i,j+1)
                elemIzquierda i j = p!(i,j-1)
                elemArriba i j = p!(i-1,j)
                elemAbajo i j = p!(i+1,j)
                elemAbDer i j = p!(i+1,j+1)
                elemArDer i j = p!(i-1,j+1)
                elemAbIzq i j = p!(i+1,j-1)
                elemArIzq i j = p!(i-1,j-1)

-- ---------------------------------------------------------------------
-- Ejercicio 11.1. Una matriz tridiagonal es aquella en la que sólo hay
-- elementos distintos de 0 en la diagonal principal o en las diagonales
-- por encima y por debajo de la diagonal principal. Por ejemplo, 
--    ( 1 2 0 0 0 0 )
--    ( 3 4 5 0 0 0 )
--    ( 0 6 7 8 0 0 )
--    ( 0 0 9 1 2 0 )
--    ( 0 0 0 3 4 5 )
--    ( 0 0 0 0 6 7 )
-- 
-- Definir la función 
--    creaTridiagonal :: Int -> Matriz Int
-- tal que (creaTridiagonal n) es la siguiente matriz tridiagonal
-- cuadrada con n filas y n columnas:
--    ( 1 1 0 0 0 0 ... 0  0  )
--    ( 1 2 2 0 0 0 ... 0  0  )
--    ( 0 2 3 3 0 0 ... 0  0  )
--    ( 0 0 3 4 4 0 ... 0  0  )
--    ( 0 0 0 4 5 5 ... 0  0  )
--    ( 0 0 0 0 5 6 ... 0  0  )
--    ( ..................... )
--    ( 0 0 0 0 0 0 ...n-1 n-1)
--    ( 0 0 0 0 0 0 ...n-1 n  )
-- Por ejemplo,
--    ghci> creaTridiagonal 4
--    array ((1,1),(4,4)) [((1,1),1),((1,2),1),((1,3),0),((1,4),0),
--                         ((2,1),1),((2,2),2),((2,3),2),((2,4),0),
--                         ((3,1),0),((3,2),2),((3,3),3),((3,4),3),
--                         ((4,1),0),((4,2),0),((4,3),3),((4,4),4)]
-- ----------------------------------------------------------------------------

creaTridiagonal :: Int -> Matriz Int
creaTridiagonal n = array ((1,1),(n,n)) [((i,j), elem i j) | i <- [1..n], j <- [1..n]]
    where
        elem i j
            | i == j        = i
            | i == (j+1)    = i-1
            | j == (i+1)    = j-1
            | otherwise     = 0

-- ----------------------------------------------------------------------------
-- Ejercicio 11.2. Definir la función 
--    esTridiagonal :: Matriz Int -> Bool
-- tal que (esTridiagonal p) se verifica si la matriz p es tridiagonal. Por 
-- ejemplo,
--    esTridiagonal (creaTridiagonal 5)               ==  True
--    esTridiagonal (listArray ((1,1),(3,3)) [1..9])  ==  False
-- ----------------------------------------------------------------------------

esTridiagonal :: Matriz Int -> Bool
esTridiagonal p
    | m /= n    = error "Matriz no cuadrada"
    | otherwise = and [p!(i,j) == (q n)!(i,j) | i <- [1..n], j <- [1..n]]
    where
        (m,n) = snd (bounds p)
        q n = creaTridiagonal n
