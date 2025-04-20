module Queue
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
where

data Queue a = Q [a]
    deriving Show
{- INV. REP.:
    * Sea Q xs: al agregar elementos, se encolan por el final de xs, y se desencolan por delante de xs.
-}

{- COSTO OPERACIONAL DE CADA FUNCIÓN:

- emptyQ        O(1)
- isEmptyQ      O(1)
- enqueue       O(n)
- firstQ        O(1)
- dequeue       O(1)

-}

------------------------------------------------ IMPLEMENTACIÓN ------------------------------------------------ 

emptyQ :: Queue a
-- PROP: Crea una cola vacía.
emptyQ = Q []


isEmptyQ :: Queue a -> Bool
-- PROP: Dada una cola indica si la cola está vacía.
isEmptyQ (Q xs) = null xs


enqueue :: a -> Queue a -> Queue a
-- PROP: Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue x (Q xs) = Q (xs ++ [x])


firstQ :: Queue a -> a
-- PROP: Dada una cola devuelve el primer elemento de la cola.
-- PRECOND: La lista de la cola no es vacía.
firstQ (Q []) = error "No hay primer elemento en una cola vacía."
firstQ (Q xs) = head xs


dequeue :: Queue a -> Queue a
-- PROP: Dada una cola la devuelve sin su primer elemento.
-- PRECOND: La lista de la cola no es vacía.
dequeue (Q []) = error "No se puede quitar el primer elemento en una cola vacía."
dequeue (Q xs) = Q (tail xs)