-- EJERCICIO 1: Priority Queue (Cola de Prioridad).

import PriorityQueue
import Map

-- EJERCICIO 1.1:

    -- INTERFAZ: emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ.


-- EJERCICIO 1.2:

heapSort :: Ord a => [a] -> [a]
-- PROP: Dada una lista, la ordena de menor a mayor utilizando una Priority Queue 
--       como estructura auxiliar.
heapSort xs = priorityQueueALista (listaAPriorityQueue xs) 

listaAPriorityQueue :: Ord a => [a] -> PriorityQueue a
listaAPriorityQueue []     = emptyPQ 
listaAPriorityQueue (x:xs) = insertPQ x (listaAPriorityQueue xs) 

priorityQueueALista :: Ord a => PriorityQueue a -> [a]
priorityQueueALista pq = if not (isEmptyPQ pq)
                            then findMinPQ pq : priorityQueueALista (deleteMinPQ pq)
                            else []


-- EJERCICIO 2: Map (Diccionario).

    -- INTERFAZ: emptyM, assocM, lookupM, deleteM, keys.


-- EJERCICIO 2.1:


valuesM :: Eq k => Map k v -> [Maybe v]
-- PROP: Obtiene los valores aso ciados a cada clave del map.
valuesM = undefineds


todasAsociadas :: Eq k => [k] -> Map k v -> Bool
-- PROP: Indica si en el map se encuentran to das las claves dadas.
todasAsociadas = undefined


listToMap :: Eq k => [(k, v)] -> Map k v
-- PROP: Convierte una lista de pares clave valor en un map.
listToMap = undefined


mapToList :: Eq k => Map k v -> [(k, v)]
-- PROP: Convierte un map en una lista de pares clave valor.
agruparEq = undefined


agruparEq :: Eq k => [(k, v)] -> Map k [v]
-- PROP: Dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave.
agruparEq = undefined


incrementar :: Eq k => [k] -> Map k Int -> Map k Int
-- PROP: Dada una lista de claves de tip o k y un map que va de k a Int, le suma uno a cada número aso ciado con dichas claves.
incrementar = undefined


mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
-- PROP: Dado dos maps se agregan las claves y valores del primer map en el segundo. Si una clave del primero existe en el segundo, es reemplazada p or la del primero
mergeMaps = undefined


-- EJERCICIO 2.2:

    -- Implementado en MapV2.hs
    -- Implementado en MapV3.hs


-- EJERCICIO 2.3:

indexar :: [a] -> Map Int a
-- PROP: Dada una lista de elementos construye un map que relaciona cada elemento con su posición en la lista.
indexar = undefined


ocurrencias :: String -> Map Char Int
-- PROP: Dado un string, devuelve un map donde las claves son los caracteres que aparecen en el string, y los valores la cantidad de veces que aparecen en el mismo
ocurrencias = undefined


-- EJERCICIO 3: MultiSet (Multiconjunto).


-- EJERCICIO 3.1: