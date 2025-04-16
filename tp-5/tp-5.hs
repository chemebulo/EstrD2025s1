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

-- El costo operacional de la función "partir" es O(n^2).


minimo :: Ord a => [a] -> a
minimo [x]    = x
minimo (x:xs) = min x (minimo xs)

-- El costo operacional de la función "minimo" es O(n^2).


sacar :: Eq a => a -> [a] -> [a]
sacar n []     = []
sacar n (x:xs) = if n == x
                    then xs
                    else x : sacar n xs

-- El costo operacional de la función "sacar" es O(n).


ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar xs = let m = minimo xs
             in  m : ordenar (sacar m xs)

-- El costo operacional de la función "ordenar" es O(n^2).


-- EJERCICIO 2: Set (Conjunto).

import SetV1

-- EJERCICIO 2.1:

    -- Implementado en Set.hs.


-- EJERCICIO 2.2:

-- losQuePertenecen :: Eq a => [a] -> Set a -> [a]
-- PROP: Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen al conjunto.



-- sinRepetidos :: Eq a => [a] -> [a]
-- PROP: Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.



-- unirTodos :: Eq a => Tree (Set a) -> Set a
-- PROP: Dado un árbol de conjuntos devuelve un conjunto con la unión de todos los conjuntos del árbol.



-- EJERCICIO 2.3:




-- EJERCICIO 3: Queue (Cola).

-------------------------------------------------- INTERFAZ --------------------------------------------------

-- emptyQ :: Queue a
-- -- PROP: Crea una cola vacía.
-- 
-- isEmptyQ :: Queue a -> Bool
-- -- PROP: Dada una cola indica si la cola está vacía.
-- 
-- enqueue :: a -> Queue a -> Queue a
-- -- PROP: Dados un elemento y una cola, agrega ese elemento a la cola.
-- 
-- firstQ :: Queue a -> a
-- -- PROP: Dada una cola devuelve el primer elemento de la cola.
-- 
-- dequeue :: Queue a -> Queue a
-- -- PROP: Dada una cola la devuelve sin su primer elemento.

--------------------------------------------------------------------------------------------------------------

-- EJERCICIO 3.1:




-- EJERCICIO 3.2:




-- EJERCICIO 3.3:




-- EJERCICIO 4: Stack (Pila).

-------------------------------------------------- INTERFAZ --------------------------------------------------

-- emptyS :: Stack a
-- -- PROP: Crea una pila vacía.
-- 
-- isEmptyS :: Stack a -> Bool
-- -- PROP: Dada una pila indica si está vacía.
-- 
-- push :: a -> Stack a -> Stack a
-- -- PROP: Dados un elemento y una pila, agrega el elemento a la pila.
-- 
-- top :: Stack a -> a
-- -- PROP: Dada un pila devuelve el elemento del top e de la pila.
-- 
-- pop :: Stack a -> Stack a
-- -- PROP: Dada una pila devuelve la pila sin el primer elemento.
-- 
-- lenS :: Stack a -> Int
-- -- PROP:  Dada la cantidad de elementos en la pila. 
-- -- COSTO: O(1).

--------------------------------------------------------------------------------------------------------------
