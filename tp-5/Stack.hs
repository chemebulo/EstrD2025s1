module Stack
    (Stack, emptyST, isEmptyST, push, top, pop, lenS)
where 

data Stack a = S [a] Int
    deriving Show
{- INV. REP.:
    * Sea S xs n: al agregar elementos, se agregan al principio de xs, también y se quitan desde el principio de xs.
    * Sea S xs n: n equivale a la cantidad de elementos en xs. 
-}

{- COSTO OPERACIONAL DE CADA FUNCIÓN:

- emptyST     O(1)
- isEmptyST   O(1)
- push        O(1)
- top         O(1)
- pop         O(1)
- lenS        O(1)

-}

------------------------------------------------ IMPLEMENTACIÓN ------------------------------------------------  

emptyST :: Stack a
-- PROP: Crea una pila vacía.
emptyST = S [] 0


isEmptyST :: Stack a -> Bool
-- PROP: Dada una pila indica si está vacía.
isEmptyST (S xs n) = null xs


push :: a -> Stack a -> Stack a
-- PROP: Dados un elemento y una pila, agrega el elemento a la pila.
push x (S xs n) = S (x:xs) (n+1)


top :: Stack a -> a
-- PROP: Dada un pila devuelve el elemento del tope de la pila.
-- PRECOND: La lista de la pila no es vacía.
top (S [] n) = error "No hay primer elemento en una pila vacia."
top (S xs n) = head xs


pop :: Stack a -> Stack a
-- PROP: Dada una pila devuelve la pila sin el primer elemento.
-- PRECOND: La lista de la pila no es vacía.
pop (S [] n) = error "No se le puede quitar el primer elemento a una pila vacia."
pop (S xs n) = S (tail xs) (n-1)


lenS :: Stack a -> Int
-- PROP: Dada una pila devuelve la cantidad de elementos en la pila. 
lenS (S xs n) = n