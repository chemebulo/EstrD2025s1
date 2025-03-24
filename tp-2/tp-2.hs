-- PUNTO 1: Recursión sobre listas.

-- EJERCICIO 1.1:
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

sumatoria :: [Int] -> Int
-- PRECOND: Ninguna.
sumatoria []     = 0
sumatoria (x:xs) = x + sumatoria xs


-- EJERCICIO 1.2:

longitud :: [a] -> Int
-- PRECOND: Ninguna.
longitud []     = 0
longitud (x:xs) = 1 + longitud xs


-- EJERCICIO 1.3:

sucesores :: [Int] -> [Int]
-- PRECOND: Ninguna.
sucesores []     = []
sucesores (n:ns) = (n+1) : sucesores ns


-- EJERCICIO 1.4:

conjuncion :: [Bool] -> Bool
-- PRECOND: Ninguna.
conjuncion []     = True
conjuncion (b:bs) = b && conjuncion bs


-- EJERCICIO 1.5:

disyuncion :: [Bool] -> Bool
-- PRECOND: Ninguna.
disyuncion []     = False
disyuncion (b:bs) = b || disyuncion bs


-- EJERCICIO 1.6:

aplanar :: [[a]] -> [a]
-- PRECOND: Ninguna.
aplanar []     = []
aplanar (x:xs) = x ++ aplanar xs 


-- EJERCICIO 1.7:

pertenece :: Eq a => a -> [a] -> Bool
-- PRECOND: Ninguna.
pertenece a []     = False 
pertenece a (x:xs) = a == x || pertenece a xs


-- EJERCICIO 1.8:

apariciones :: Eq a => a -> [a] -> Int
-- PRECOND: Ninguna.
apariciones a []     = 0
apariciones a (x:xs) = if a == x
                       then 1 + apariciones a xs
                       else apariciones a xs


-- EJERCICIO 1.9:

losMenoresA :: Int -> [Int] -> [Int]
-- PRECOND: Ninguna.
losMenoresA n []     = []
losMenoresA n (ns:nss) = if ns < n
                         then ns : losMenoresA n nss
                         else losMenoresA n nss


-- EJERCICIO 1.10:



-- EJERCICIO 1.11:



-- EJERCICIO 1.12:




-- EJERCICIO 1.13:



-- EJERCICIO 1.14:





-- EJERCICIO 1.15:



-- PUNTO 2: Recusión sobre números.


-- EJERCICIO 2.1:



-- EJERCICIO 2.2:



-- EJERCICIO 2.3:



-- EJERCICIO 2.4:



-- EJERCICIO 2.5:



-- PUNTO 3: Registros.


-- EJERCICIO 3.1:



-- EJERCICIO 3.2:


-- EJERCICIO 3.3: