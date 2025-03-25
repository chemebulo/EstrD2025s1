-- PUNTO 1: Recursión sobre listas.

-- EJERCICIO 1.1:

sumatoria :: [Int] -> Int
-- PRECOND: Ninguna.
sumatoria []     = 0
sumatoria (n:ns) = n + sumatoria ns


-- EJERCICIO 1.2:

longitud :: [a] -> Int
-- PRECOND: Ninguna.
longitud []     = 0
longitud (x:xs) = 1 + longitud xs


-- EJERCICIO 1.3:

sucesores :: [Int] -> [Int]
-- PRECOND: Ninguna.
sucesores []     = []
sucesores (n:ns) = n+1 : sucesores ns


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
pertenece y []     = False
pertenece y (x:xs) = y == x || pertenece y xs


-- EJERCICIO 1.8:

apariciones :: Eq a => a -> [a] -> Int
-- PRECOND: Ninguna.
apariciones y []     = 0
apariciones y (x:xs) = if y == x
                       then 1 + apariciones y xs
                       else apariciones y xs


-- EJERCICIO 1.9:

losMenoresA :: Int -> [Int] -> [Int]
-- PRECOND: Ninguna.
losMenoresA n []     = []
losMenoresA n (ns:nss) = if ns < n
                         then ns : losMenoresA n nss
                         else losMenoresA n nss


-- EJERCICIO 1.10:

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
-- PRECOND: El número dado debe ser mayor o igual a 0.
lasDeLongitudMayorA n []     = []
lasDeLongitudMayorA n (x:xs) = if longitud x > n
                               then x : lasDeLongitudMayorA n xs
                               else lasDeLongitudMayorA n xs


-- EJERCICIO 1.11:

agregarAlFinal :: [a] -> a -> [a]
-- PRECOND: Ninguna.
agregarAlFinal []     y = [y]
agregarAlFinal (x:xs) y = x : agregarAlFinal xs y


-- EJERCICIO 1.12:

agregar :: [a] -> [a] -> [a]
-- PRECOND: Ninguna.
agregar []     []     = []
agregar (x:xs) []     = x:xs
agregar []     (y:ys) = y:ys
agregar (x:xs) (y:ys) = x : agregar xs (y:ys)


-- EJERCICIO 1.13:

reversa :: [a] -> [a]
-- PRECOND: Ninguna.
reversa []     = []
reversa (x:xs) = agregar (reversa xs) [x]


-- EJERCICIO 1.14:

zipMaximos :: [Int] -> [Int] -> [Int]
-- PRECOND: Ninguna.
zipMaximos []     ys     = ys
zipMaximos xs     []     = xs
zipMaximos (x:xs) (y:ys) = if x >= y
                           then x : zipMaximos xs ys
                           else y : zipMaximos xs ys


-- EJERCICIO 1.15:

elMinimo :: Ord a => [a] -> a
-- PRECOND: La lista dada no puede ser vacía.
elMinimo (x:[])  =  x
elMinimo (x:xs) = if x < elMinimo xs
                  then x
                  else elMinimo xs


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