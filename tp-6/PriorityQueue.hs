module PriorityQueue
    (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)
where

data PriorityQueue a = PQ [a]
    deriving Show
{- INV. REP.:
    * Sea PQ xs: se considera el elemento de máxima prioridad de xs, 
                 al mínimmo elemento de xs.
-}

{- COSTO OPERACIONAL DE CADA FUNCIÓN:

- emptyPQ       O(1)
- isEmptyPQ     O(1)
- insertPQ      O(n^2)
- findMinPQ     O(1)
- deleteMinPQ   O(n)

-}

------------------------------------------------ IMPLEMENTACIÓN ------------------------------------------------ 

emptyPQ :: PriorityQueue a
-- PROP: Deuelve una priority queue vacía.
emptyPQ = PQ []


isEmptyPQ :: PriorityQueue a -> Bool
-- PROP: Indica si la priority queue está vacía.
isEmptyPQ (PQ xs) = null xs


insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
-- PROP: Inserta un elemento en la priority queue.
insertPQ x (PQ xs) = PQ (insertarEnXS x xs)

insertarEnXS :: Ord a => a -> [a] -> [a]
insertarEnXS x []     = [x]
insertarEnXS x (y:ys) = if (x <= y) && esElMinimoDe y ys
                           then [x, y] ++ ys
                           else y : insertarEnXS x ys

esElMinimoDe :: Ord a => a -> [a] -> Bool
esElMinimoDe x []     = True
esElMinimoDe x (y:ys) = x <= y || esElMinimoDe x ys


findMinPQ :: Ord a => PriorityQueue a -> a
-- PROP: Devuelve el elemento más prioritario (el mínimo) de la priority queue.
-- PRECOND: Parcial en caso de priority queue vacía.
findMinPQ (PQ []) = error "No hay un mínimo elemento en una priority queue vacía."
findMinPQ (PQ xs) = head xs


deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
-- PROP: Devuelve una priority queue sin el elemento más prioritario (el mínimo).
-- PRECOND: Parcial en caso de priority queue vacía.
deleteMinPQ (PQ []) = error "No se puede eliminar el mínimo elemento en una priority queue vacía."
deleteMinPQ (PQ xs) = PQ (tail xs)