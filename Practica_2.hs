-- PUNTO 1: Recursión sobre listas.

-- EJERCICIO 1.1:

sumatoria :: [Int] -> Int
-- PRECOND: Ninguna.
sumatoria []     = 0
sumatoria (x:xs) = x + (sumatoria xs)


-- EJERCICIO 1.2:

longitud :: [a] -> Int
-- PRECOND: Ninguna.
longitud []     = 0
longitud (x:xs) = 1 + longitud xs


-- EJERCICIO 1.3:

sucesores :: [Int] -> [Int]
-- PRECOND: La lista dada no puede ser vacía.
sucesores []     = 0
sucesores (x:xs) = (x+1) sucesores xs

-- EJERCICIO 1.4:



-- EJERCICIO 1.5:




-- EJERCICIO 1.6:





-- EJERCICIO 1.7:




-- EJERCICIO 1.8:




-- EJERCICIO 1.9:



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