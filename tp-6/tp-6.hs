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

-- (assocM "NRJ540" 110 (assocM "NNO999" 500 (assocM "MVO118" 500 emptyM)))
-- [("MVO118", 500), ("NN0999", 350), ("NRJ540", 110)]

-- EJERCICIO 2.1:

valuesM :: Eq k => Map k v -> [Maybe v]
-- PROP: Obtiene los valores asociados a cada clave del map.
-- O(n^2).
valuesM m = valoresDeKeysEn (keys m) m

valoresDeKeysEn :: Eq k => [k] -> Map k v -> [Maybe v]
valoresDeKeysEn []     m = []
valoresDeKeysEn (k:ks) m = lookupM k m : valoresDeKeysEn ks m


todasAsociadas :: Eq k => [k] -> Map k v -> Bool
-- PROP: Indica si en el map se encuentran todas las claves dadas.
-- O(n^2).
todasAsociadas []     m = True
todasAsociadas (k:ks) m = estaAsociadoEn k m && todasAsociadas ks m

estaAsociadoEn :: Eq k => k -> Map k v -> Bool
estaAsociadoEn k m = elem k (keys m)


listToMap :: Eq k => [(k, v)] -> Map k v
-- PROP: Convierte una lista de pares clave valor en un map.
-- O(n^2).
listToMap []          = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)


mapToList :: Eq k => Map k v -> [(k, v)]
-- PROP: Convierte un map en una lista de pares clave valor.
-- O(n).
mapToList m = mapALista m (keys m)

mapALista :: Eq k => Map k v -> [k] -> [(k, v)]
mapALista m []     = []
mapALista m (k:ks) = let v = fromJust (lookupM k m)
                     in (k, v) : mapALista m ks

fromJust :: Maybe a -> a
fromJust Nothing   = error "No tendria que dar esto."
fromJust (Just x)  = x


agruparEq :: Eq k => [(k, v)] -> Map k [v]
-- PROP: Dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave.
-- O().
agruparEq kvs = agruparValuesAKeys (soloKeys kvs) kvs

agruparValuesAKeys :: Eq k => [k] -> [(k, v)] -> Map k [v]
agruparValuesAKeys []     kvs = emptyM
agruparValuesAKeys (k:ks) kvs = assocM k (agruparValuesDeKey k kvs) (agruparValuesAKeys ks kvs)

agruparValuesDeKey :: Eq k => k -> [(k, v)] -> [v]
agruparValuesDeKey nk []          = []
agruparValuesDeKey nk ((k,v):kvs) = if nk == k
                                       then v : agruparValuesDeKey nk kvs
                                       else agruparValuesDeKey nk kvs

soloKeys :: Eq k => [(k, v)] -> [k]
soloKeys []          = []
soloKeys ((k,v):kvs) = if not (estaEnPares k kvs)
                          then k : soloKeys kvs
                          else soloKeys kvs

estaEnPares :: Eq k => k -> [(k, v)] -> Bool
estaEnPares nk []          = False
estaEnPares nk ((k,v):kvs) = nk == k || estaEnPares nk kvs


incrementar :: Eq k => [k] -> Map k Int -> Map k Int
-- PROP: Dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a cada número asociado con dichas claves.
-- O(n^2).
incrementar []     m = m
incrementar (k:ks) m = let v = fromJust (lookupM k m)
                       in if estaAsociadoEn k m
                             then assocM k (v + 1) (incrementar ks m)
                             else incrementar ks m


mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
-- PROP: Dado dos maps se agregan las claves y valores del primer map en el segundo. Si una clave del primero  
--       existe en el segundo, es reemplazada por la del primero.
-- O().
mergeMaps m1 m2 = agregarMapAMap (keys m1) m1 m2

agregarMapAMap :: Eq k => [k] -> Map k v -> Map k v -> Map k v
agregarMapAMap []     m1 m2 = m2
agregarMapAMap (k:ks) m1 m2 = let v = fromJust (lookupM k m1)
                              in agregarMapAMap ks m1 (assocM k v m2)


-- EJERCICIO 2.2:

    -- Implementado en MapV2.hs
    -- Implementado en MapV3.hs


-- EJERCICIO 2.3:

indexar :: [a] -> Map Int a
-- PROP: Dada una lista de elementos construye un map que relaciona cada elemento con su posición en la lista.
-- O(n).
indexar xs = indexarXS 1 xs

indexarXS :: Int -> [a] -> Map Int a
indexarXS n []     = emptyM
indexarXS n (x:xs) = assocM n x (indexarXS (n+1) xs)


ocurrencias :: String -> Map Char Int
-- PROP: Dado un string, devuelve un map donde las claves son los caracteres que aparecen en el string, y los 
--       valores la cantidad de veces que aparecen en el mismo.
-- O().
ocurrencias s = ocurrenciasDe (sinRepeticiones s) s

ocurrenciasDe :: String -> String -> Map Char Int
ocurrenciasDe []     s = emptyM
ocurrenciasDe (c:cs) s = let num = aparicionesDeEn c s
                         in assocM c num (ocurrenciasDe cs s)

aparicionesDeEn :: Char -> String -> Int
aparicionesDeEn x []     = 0
aparicionesDeEn x (c:cs) = if x == c
                              then 1 + aparicionesDeEn x cs
                              else aparicionesDeEn x cs

sinRepeticiones :: Eq a => [a] -> [a]
sinRepeticiones []     = []
sinRepeticiones (x:xs) = if elem x xs
                            then sinRepeticiones xs
                            else x : sinRepeticiones xs


-- EJERCICIO 3: MultiSet (Multiconjunto).


-- EJERCICIO 3.1: