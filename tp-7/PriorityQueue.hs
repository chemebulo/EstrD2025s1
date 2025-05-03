module PriorityQueue
    (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)
where

data PriorityQueue a = PQ [a]
    deriving Show
{- INV. REP.:
    * Sea PQ xs: Se considera el elemento de máxima prioridad de xs, al mínimmo elemento de xs.
-}

{- COSTO OPERACIONAL DE CADA FUNCIÓN:

- emptyPQ       O(1)
- isEmptyPQ     O(1)
- insertPQ      O(n)
- findMinPQ     O(1)
- deleteMinPQ   O(1)

-}

-- #################################################### IMPLEMENTACIÓN ####################################################

emptyPQ :: PriorityQueue a
-- PROP: Deuelve una priority queue vacía.
    -- COSTO: O(1).
    -- Siendo en el peor caso de costo constante por el hecho de solamente utilizar el constructor con una lista vacía.
emptyPQ = PQ []


isEmptyPQ :: PriorityQueue a -> Bool
-- PROP: Indica si la priority queue está vacía.
    -- COSTO: O(1).
    -- Siendo que utiliza "null" de costo constante en la lista dada, la función resulta de dicho costo.
isEmptyPQ (PQ xs) = null xs


insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
-- PROP: Inserta un elemento en la priority queue.
    -- COSTO: O(n).
    -- Siendo n la cantidad de elementos de la lista de la priority queue, se utiliza la función "insertarEnXS" de costo
    -- lineal en el peor caso, con la lista y el elemento dado. 
insertPQ x (PQ xs) = PQ (insertarEnXS x xs)


findMinPQ :: Ord a => PriorityQueue a -> a
-- PROP: Devuelve el elemento más prioritario (el mínimo) de la priority queue.
-- PRECOND: Parcial en caso de priority queue vacía.
    -- COSTO: O(1).
    -- Siendo que utiliza la función "head" de costo constante en el peor caso para la lista dada. 
findMinPQ (PQ []) = error "No hay un minimo elemento en una priority queue vacia."
findMinPQ (PQ xs) = head xs


deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
-- PROP: Devuelve una priority queue sin el elemento más prioritario (el mínimo).
-- PRECOND: Parcial en caso de priority queue vacía.
    -- COSTO: O(1).
    -- Siendo Siendo n la cantidad de elementos de la lista de la priority queue, se utiliza la función "tail" de costo
    -- constante en el peor caso.
deleteMinPQ (PQ []) = error "No se puede eliminar el minimo elemento en una priority queue vacia."
deleteMinPQ (PQ xs) = PQ (tail xs)

-- #################################################### AUXILIARES ####################################################

insertarEnXS :: Ord a => a -> [a] -> [a]
    -- COSTO: O(n).
    -- Siendo n la cantidad de elementos de la lista dada, se utiliza la función "esElMinimoDe" de costo lineal para
    -- cada elemento de la lista dada. Además se realiza un "++" entre dos listas (en el caso que sea el mínimo en ys). 
    -- En el peor caso, llegando al final de la lista dada e insertando el elemento dado, resulta de costo lineal.
insertarEnXS x []     = [x]
insertarEnXS x (y:ys) = if (x <= y) && esElMinimoDe y ys
                           then [x, y] ++ ys
                           else y : insertarEnXS x ys

esElMinimoDe :: Ord a => a -> [a] -> Bool
    -- COSTO: O(n).
    -- Siendo n la cantidad de elementos de la lista dada, por cada uno se realiza una comparacion de costo constante
    -- con el elemento dado, y en el peor caso la función resulta de costo lineal, ya que el peor caso es llegar al 
    -- de la lista dada. 
esElMinimoDe x []     = True
esElMinimoDe x (y:ys) = x <= y || esElMinimoDe x ys