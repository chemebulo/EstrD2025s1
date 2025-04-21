module QueueV3
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
where

data Queue a = Q [a] [a]
    deriving Show
{- INV. REP.:
    * Sea Q fs bs: si fs se encuentra vacía, entonces la cola se encuentra vacía.
-}

{- COSTO OPERACIONAL DE CADA FUNCIÓN:

- emptyQ        O(1)
- isEmptyQ      O(1)
- enqueue       O(1)
- firstQ        O(1)
- dequeue       O(1)

-}

------------------------------------------------ IMPLEMENTACIÓN ------------------------------------------------  

emptyQ :: Queue a
-- PROP: Crea una cola vacía.
emptyQ = Q [] []


isEmptyQ :: Queue a -> Bool
-- PROP: Dada una cola indica si la cola está vacía.
isEmptyQ (Q fs bs) = null fs


enqueue :: a -> Queue a -> Queue a
-- PROP: Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue x (Q [] bs) = Q [x] []
enqueue x (Q fs bs) = Q fs  (x:bs)


firstQ :: Queue a -> a
-- PROP: Dada una cola devuelve el primer elemento de la cola.
-- PRECOND: La lista de la cola no es vacía.
firstQ (Q [] _ ) = error "No hay primer elemento en una cola vacia."
firstQ (Q fs bs) = head fs


dequeue :: Queue a -> Queue a
-- PROP: Dada una cola la devuelve sin su primer elemento.
-- PRECOND: La lista de la cola no es vacía.
dequeue (Q []  []) = error "No se puede quitar el primer elemento en una cola vacia."
dequeue (Q [f] []) = Q [] []
dequeue (Q [f] bs) = Q (reverse bs) []
dequeue (Q fs  bs) = Q (tail fs) bs