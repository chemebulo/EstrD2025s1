import Set
import QueueV3
import Stack

-------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------

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


sinRepetidosL :: Eq a => [a] -> [a]
sinRepetidosL []     = []
sinRepetidosL (x:xs) = if pertenece x xs
                         then sinRepetidosL xs
                         else x : sinRepetidosL xs

-- El costo operacional de la función "sinRepetidosL" es O(n^2).


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

    -- INTERFAZ: emptyS, addS, belongs, sizeS, removeS, unionS, setToList.

-- EJERCICIO 2.1:

    -- Implementado en Set.hs.

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

-- EJERCICIO 2.2:

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
-- PROP: Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen al conjunto.
losQuePertenecen []     s = []
losQuePertenecen (x:xs) s = if belongs x s
                               then x : losQuePertenecen xs s
                               else losQuePertenecen xs s


sinRepetidos :: Eq a => [a] -> [a]
-- PROP: Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidos xs = setToList (listaAConjunto xs)

listaAConjunto :: Eq a => [a] -> Set a
listaAConjunto []     = emptyS
listaAConjunto (x:xs) = addS x (listaAConjunto xs)


unirTodos :: Eq a => Tree (Set a) -> Set a
-- PROP: Dado un árbol de conjuntos devuelve un conjunto con la unión de todos los conjuntos del árbol.
unirTodos EmptyT          = emptyS
unirTodos (NodeT s t1 t2) = unionS s (unionS (unirTodos t1) (unirTodos t2))


-- EJERCICIO 2.3:

losQuePertenecen' :: Eq a => [a] -> Set a -> [a]
-- PROP: Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen al conjunto.
losQuePertenecen' []     s = []
losQuePertenecen' (x:xs) s = if belongs x s
                                then x : losQuePertenecen' xs s
                                else losQuePertenecen' xs s


sinRepetidos' :: Eq a => [a] -> [a]
-- PROP: Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidos' xs = setToList (listaAConjunto xs)


unirTodos' :: Eq a => Tree (Set a) -> Set a
-- PROP: Dado un árbol de conjuntos devuelve un conjunto con la unión de todos los conjuntos del árbol.
unirTodos' EmptyT          = emptyS
unirTodos' (NodeT s t1 t2) = unionS s (unionS (unirTodos' t1) (unirTodos' t2))


{- COSTO OPERACIONAL DE CADA IMPLEMENTACIÓN:

------------------------------------------------------
|   SET (SIN REPETIDOS)  |    SET (CON REPETIDOS)    |
|------------------------|---------------------------|
|  emptyS        O(1)    |   emptyS        O(1)      |
|  addS          O(n^2)  |   addS          O(1)      |
|  belongs       O(n)    |   belongs       O(n)      |
|  sizeS         O(1)    |   sizeS         O(n^2)    |   
|  removeS       O(n^2)  |   removeS       O(n^2)    |   
|  unionS        O(n^2)  |   unionS        O(1)      |
|  setToList     O(1)    |   setToList     O(n)      |
------------------------------------------------------

-}


-- EJERCICIO 3: Queue (Cola).

    -- INTERFAZ: emptyQ, isEmptyQ, enqueue, firstQ, dequeue.

-- EJERCICIO 3.1:

    -- Implementado en Queue.hs.


-- EJERCICIO 3.2:

    -- Implementado en QueueV2.hs.


-- EJERCICIO 3.3:

lengthQ :: Queue a -> Int
-- PROP: Cuenta la cantidad de elementos de la cola.
lengthQ q = if isEmptyQ q
               then 0
               else 1 + lengthQ (dequeue q)


queueToList :: Queue a -> [a]
-- PROP: Dada una cola devuelve la lista con los mismos elementos, donde el orden de la lista es el de la cola.
-- Nota: Chequear que los elementos queden en el orden correcto.
queueToList q = if isEmptyQ q
                   then []
                   else firstQ q : queueToList (dequeue q)


unionQ :: Queue a -> Queue a -> Queue a
-- PROP: Inserta todos los elementos de la segunda cola en la primera
unionQ q1 q2 = if isEmptyQ q2
                  then q1
                  else enqueue (firstQ q2) (unionQ q1 (dequeue q2))


{- COSTO OPERACIONAL DE CADA IMPLEMENTACIÓN:

------------------------------------------------------
|         QUEUE.V1       |          QUEUE.V2         |
|------------------------|---------------------------|
|   emptyQ        O(1)   |     emptyQ        O(1)    |
|   isEmptyQ      O(1)   |     isEmptyQ      O(1)    |
|   enqueue       O(n)   |     enqueue       O(1)    |
|   firstQ        O(1)   |     firstQ        O(n)    |
|   dequeue       O(1)   |     dequeue       O(n)    |
------------------------------------------------------

-}


-- EJERCICIO 4: Stack (Pila).

    -- INTERFAZ: emptyST, isEmptyST, push, top, pop, lenS.

-- EJERCICIO 4.1:

apilar :: [a] -> Stack a
-- PROP: Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar []     = emptyST
apilar (x:xs) = push x (apilar xs)


desapilar :: Stack a -> [a]
-- PROP: Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar st = if not (isEmptyST st)
                  then top st : desapilar (pop st)
                  else []


insertarEnPos :: Int -> a -> Stack a -> Stack a
-- PROP: Dada una posición válida en la stack y un elemento, ubica dicho elemento en dicha posición 
--       (se desapilan elementos hasta dicha posición y se inserta en ese lugar).
-- PRECOND: La posicion dada existe dentro del stack dado.
insertarEnPos 0 x st = push x st
insertarEnPos n x st = push (top st) (insertarEnPos (n-1) x (pop st))


-- EJERCICIO 4.2:

    -- Implementado en Stack.hs

    {- COSTO OPERACIONAL LA IMPLEMENTACION

          --------------------------
          |          STACK         |
          |------------------------|
          |    emptyST     O(1)    |
          |    isEmptyST   O(1)    |
          |    push        O(1)    |
          |    top         O(1)    |
          |    pop         O(1)    |
          |    lenS        O(1)    |
          --------------------------

    -}


-- EJERCICIO 5: Queue con Dos Listas.

    -- Implementado en QueueV3.

    {- COSTO OPERACIONAL DE CADA IMPLEMENTACIÓN:

    ----------------------------------------------------------------------------------
    |         QUEUE.V1       |          QUEUE.V2         |          QUEUE.V3         |
    |------------------------|---------------------------|---------------------------|
    |   emptyQ        O(1)   |     emptyQ        O(1)    |     emptyQ        O(1)    |
    |   isEmptyQ      O(1)   |     isEmptyQ      O(1)    |     isEmptyQ      O(1)    |
    |   enqueue       O(n)   |     enqueue       O(1)    |     enqueue       O(1)    |
    |   firstQ        O(1)   |     firstQ        O(n)    |     firstQ        O(1)    |
    |   dequeue       O(1)   |     dequeue       O(n)    |     dequeue       O(1)    |
    ----------------------------------------------------------------------------------

    -}