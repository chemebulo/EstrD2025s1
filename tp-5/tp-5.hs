-- EJERCICIO 1: Cálculo de costos.

head' :: [a] -> a
head' (x:xs) = x

-- El costo operacional de la función "head'" es O(1).


sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

-- El costo operacional de la función "sumar" es O(1).


factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- El costo operacional de la función "factorial" es O(n).


longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

-- El costo operacional de la función "longitud" es O(n).


factoriales :: [Int] -> [Int]
factoriales []     = []
factoriales (x:xs) = factorial x : factoriales xs

-- El costo operacional de la función "factoriales" es O(n^2).


pertenece :: Eq a => a -> [a] -> Bool
pertenece n []     = False
pertenece n (x:xs) = n == x || pertenece n xs

-- El costo operacional de la función "pertenece" es O(n).


sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos []     = []
sinRepetidos (x:xs) = if pertenece x xs
                         then sinRepetidos xs
                         else x : sinRepetidos xs

-- El costo operacional de la función "sinRepetidos" es O(n^2).


append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys

-- El costo operacional de la función "append" es O(n).


concatenar :: [String] -> String
concatenar []     = []
concatenar (x:xs) = x ++ concatenar xs

-- El costo operacional de la función "concatenar" es O(n).


takeN :: Int -> [a] -> [a]
takeN 0 xs     = xs
takeN n []     = []
takeN n (x:xs) = x : takeN (n-1) xs

-- El costo operacional de la función "takeN" es O(n).


dropN :: Int -> [a] -> [a]
dropN 0 xs     = xs
dropN n []     = []
dropN n (x:xs) = dropN (n-1) xs

-- El costo operacional de la función "dropN" es O(n).


partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)

-- El costo operacional de la función "partir" es O().


minimo :: Ord a => [a] -> a
minimo [x]    = x
minimo (x:xs) = min x (minimo xs)

-- El costo operacional de la función "minimo" es O().


sacar :: Eq a => a -> [a] -> [a]
sacar n []     = []
sacar n (x:xs) = if n == x
                    then xs
                    else x : sacar n xs

-- El costo operacional de la función "sacar" es O().


ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs = let m = minimo xs
             in  m : ordenar (sacar m xs)

-- El costo operacional de la función "ordenar" es O().


-- EJERCICIO 2: Set (Conjunto).




-- EJERCICIO 3: Queue (Cola).




-- EJERCICIO 4: Stack (Pila).



