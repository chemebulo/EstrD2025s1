-- EJERCICIO 1: Priority Queue (Cola de Prioridad).

import PriorityQueue
import Map
import MultiSet

-- EJERCICIO 1.1:

    -- INTERFAZ: emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ.

    {- COSTO OPERACIONAL DE CADA FUNCIÓN:

    --------------------------
    |    PRIORITY QUEUE V1   |
    |------------------------|
    |   emptyPQ       O(1)   |
    |   isEmptyPQ     O(1)   |
    |   insertPQ      O(n)   |
    |   findMinPQ     O(1)   |
    |   deleteMinPQ   O(1)   |
    --------------------------

    -}


-- EJERCICIO 1.2:

heapSort :: Ord a => [a] -> [a]
-- PROP: Dada una lista, la ordena de menor a mayor utilizando una Priority Queue como estructura auxiliar.
    -- COSTO: O(n^2).
    -- Siendo n la cantidad de elementos de la lista dada, se utiliza en toda la lista "listaAPriorityQueue" de costo cuadrático,
    -- y para la priority queue resultante "priorityQueueALista" de costo lineal. Esto resulta en que "heapSort" sea de costo cuadrático,
    -- ya que es el peor caso dentro de los costos de operación de las funciones utilizadas. 
heapSort xs = priorityQueueALista (listaAPriorityQueue xs)

priorityQueueALista :: Ord a => PriorityQueue a -> [a]
    -- COSTO: O(n).
    -- Siendo n la cantidad de elementos de la priority queue, para cada elemento se utiliza "findMinPQ" de costo costante y se elimina 
    -- el mínimo de la lista dada utilizando "deleteMinPQ" de costo constante; y también se utiliza "isEmptyPQ" de costo constante.
    -- Esto resulta en que el costo de "priorityQueueALista" sea lineal, ya que realiza operaciones constantes por cada elemento de 
    -- la priority queue.
priorityQueueALista pq = if not (isEmptyPQ pq)
                            then findMinPQ pq : priorityQueueALista (deleteMinPQ pq)
                            else []

listaAPriorityQueue :: Ord a => [a] -> PriorityQueue a
    -- COSTO: O(n^2). 
    -- Siendo n la cantidad de elementos de la lista dada, se utiliza "insertPQ" de costo lineal en el peor caso en cada uno de ellos.
    -- Esto resulta en que el costo de "listaAPriorityQueue" sea de costo cuadrático, ya que el costo de insertar cada elemento de la 
    -- lista dada en la priority queue, es lineal.
listaAPriorityQueue []     = emptyPQ
listaAPriorityQueue (x:xs) = insertPQ x (listaAPriorityQueue xs)


-- EJERCICIO 2: Map (Diccionario).

    -- INTERFAZ: emptyM, assocM, lookupM, deleteM, keys.

    {- COSTO OPERACIONAL DE CADA IMPLEMENTACIÓN:

    --------------------------------------------------------------
    |       MAP_V1     |        MAP_V2       |      MAP_V3       |
    |------------------|---------------------|-------------------|
    |  emptyM    O(1)  |   emptyM    O(1)    |   emptyM    O(1)  |
    |  assocM    O(n)  |   assocM    O(1)    |   assocM    O(n)  |
    |  lookupM   O(n)  |   lookupM   O(n)    |   lookupM   O(n)  |
    |  deleteM   O(n)  |   deleteM   O(n)    |   deleteM   O(n)  |
    |  keys      O(n)  |   keys      O(n^2)  |   keys      O(1)  |
    --------------------------------------------------------------

    -}


-- EJERCICIO 2.1:

    -- EL ANÁLISIS DE TODAS ESTAS FUNCIONES ES EN BASE A MAP_V1.

valuesM :: Eq k => Map k v -> [Maybe v]
-- PROP: Obtiene los valores asociados a cada clave del map.
    -- COSTO: O(n*m).
    -- Siendo n la cantidad de claves en el map, se obtienen las claves del map utilizando la operación "keys" de
    -- costo lineal. Posteriormente se obtienen los valores de cada clave con "valoresDeKeysEn" sobre en que también
    -- es lineal por cada lookupM. Esto resulta en que el costo total es cuadrático.
valuesM m = valoresDeKeysEn (keys m) m

valoresDeKeysEn :: Eq k => [k] -> Map k v -> [Maybe v]
    -- COSTO: O(n*m).
    -- Siendo m la longitud de la lista de claves dada y n la cantidad de claves en el map, se utiliza la operación
    -- "lookupM" de costo lineal por cada clave de la lista. Esto termina resultando que el costo total sea 'm*n'.
valoresDeKeysEn []     m = []
valoresDeKeysEn (k:ks) m = lookupM k m : valoresDeKeysEn ks m


todasAsociadas :: Eq k => [k] -> Map k v -> Bool
-- PROP: Indica si en el map se encuentran todas las claves dadas.
    -- COSTO: O(n*m).
    -- Siendo m la longitud de la lista de claves dada y n la cantidad de claves en el map, se utiliza "estaAsociadoEn" de
    -- costo lineal por cada clave. En el peor caso, se recorren todas las claves, resultando un costo total de 'm*n'.
todasAsociadas []     m = True
todasAsociadas (k:ks) m = estaAsociadoEn k m && todasAsociadas ks m

estaAsociadoEn :: Eq k => k -> Map k v -> Bool
    -- COSTO: O(n).
    -- Siendo que "keys m" es de costo lineal, se utiliza "elem" para buscar la clave (de costo lineal). Tanto "keys" como "elem"
    -- son operaciones lineales, lo que termina resultando que el costo total sea lineal.
estaAsociadoEn k m = elem k (keys m)


listToMap :: Eq k => [(k, v)] -> Map k v
-- PROP: Convierte una lista de pares clave valor en un map.
    -- COSTO: O().
    -- Siendo 
listToMap []          = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)


mapToList :: Eq k => Map k v -> [(k, v)]
-- PROP: Convierte un map en una lista de pares clave valor.
    -- COSTO: O().
    -- Siendo 
mapToList m = mapALista m (keys m)

mapALista :: Eq k => Map k v -> [k] -> [(k, v)]
    -- COSTO: O().
    -- Siendo 
mapALista m []     = []
mapALista m (k:ks) = let v = fromJust (lookupM k m)
                     in (k, v) : mapALista m ks

fromJust :: Maybe a -> a
    -- COSTO: O().
    -- Siendo 
fromJust Nothing   = error "No tendria que dar esto."
fromJust (Just x)  = x


agruparEq :: Eq k => [(k, v)] -> Map k [v]
-- PROP: Dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave.
    -- COSTO: O().
    -- Siendo 
agruparEq kvs = agruparValuesAKeys (soloKeys kvs) kvs

agruparValuesAKeys :: Eq k => [k] -> [(k, v)] -> Map k [v]
    -- COSTO: O().
    -- Siendo 
agruparValuesAKeys []     kvs = emptyM
agruparValuesAKeys (k:ks) kvs = assocM k (agruparValuesDeKey k kvs) (agruparValuesAKeys ks kvs)

agruparValuesDeKey :: Eq k => k -> [(k, v)] -> [v]
    -- COSTO: O().
    -- Siendo 
agruparValuesDeKey nk []          = []
agruparValuesDeKey nk ((k,v):kvs) = if nk == k
                                       then v : agruparValuesDeKey nk kvs
                                       else agruparValuesDeKey nk kvs

soloKeys :: Eq k => [(k, v)] -> [k]
    -- COSTO: O().
    -- Siendo 
soloKeys []          = []
soloKeys ((k,v):kvs) = if not (estaEnPares k kvs)
                          then k : soloKeys kvs
                          else soloKeys kvs

estaEnPares :: Eq k => k -> [(k, v)] -> Bool
    -- COSTO: O().
    -- Siendo 
estaEnPares nk []          = False
estaEnPares nk ((k,v):kvs) = nk == k || estaEnPares nk kvs


incrementar :: Eq k => [k] -> Map k Int -> Map k Int
-- PROP: Dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a cada número asociado con dichas claves.
    -- COSTO: O().
    -- Siendo 
incrementar []     m = m
incrementar (k:ks) m = let v = fromJust (lookupM k m)
                       in if estaAsociadoEn k m
                             then assocM k (v + 1) (incrementar ks m)
                             else incrementar ks m


mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
-- PROP: Dado dos maps se agregan las claves y valores del primer map en el segundo. Si una clave del primero existe en el segundo,
--       es reemplazada por la del primero.
    -- COSTO: O().
    -- Siendo 
mergeMaps m1 m2 = agregarMapAMap (keys m1) m1 m2

agregarMapAMap :: Eq k => [k] -> Map k v -> Map k v -> Map k v
    -- COSTO: O().
    -- Siendo 
agregarMapAMap []     m1 m2 = m2
agregarMapAMap (k:ks) m1 m2 = let v = fromJust (lookupM k m1)
                              in agregarMapAMap ks m1 (assocM k v m2)


-- EJERCICIO 2.2:

    -- Implementado en MapV2.hs
    -- Implementado en MapV3.hs


-- EJERCICIO 2.3:

indexar :: [a] -> Map Int a
-- PROP: Dada una lista de elementos construye un map que relaciona cada elemento con su posición en la lista.
    -- COSTO: O().
    -- Siendo 
indexar xs = indexarXS 1 xs

indexarXS :: Int -> [a] -> Map Int a
    -- COSTO: O().
    -- Siendo 
indexarXS n []     = emptyM
indexarXS n (x:xs) = assocM n x (indexarXS (n+1) xs)


ocurrencias :: String -> Map Char Int
-- PROP: Dado un string, devuelve un map donde las claves son los caracteres que aparecen en el string, y los 
--       valores la cantidad de veces que aparecen en el mismo.
    -- COSTO: O().
    -- Siendo 
ocurrencias s = ocurrenciasDe (sinRepeticiones s) s

ocurrenciasDe :: String -> String -> Map Char Int
    -- COSTO: O().
    -- Siendo 
ocurrenciasDe []     s = emptyM
ocurrenciasDe (c:cs) s = let num = aparicionesDeEn c s
                         in assocM c num (ocurrenciasDe cs s)

aparicionesDeEn :: Char -> String -> Int
    -- COSTO: O().
    -- Siendo 
aparicionesDeEn x []     = 0
aparicionesDeEn x (c:cs) = if x == c
                              then 1 + aparicionesDeEn x cs
                              else aparicionesDeEn x cs

sinRepeticiones :: Eq a => [a] -> [a]
    -- COSTO: O().
    -- Siendo 
sinRepeticiones []     = []
sinRepeticiones (x:xs) = if elem x xs
                            then sinRepeticiones xs
                            else x : sinRepeticiones xs


-- EJERCICIO 3: MultiSet (Multiconjunto).

    -- INTERFAZ: emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList.

    {- COSTO OPERACIONAL DE CADA FUNCIÓN:

    --------------------------------
    |         MULTISET V1          |
    |------------------------------|
    |   emptyMS           O(1)     |
    |   addMS             O(n)     |
    |   ocurrencesMS      O(n)     |
    |   unionMS           O(n^2)   |
    |   intersectionMS    O(n^2)   |
    |   multiSetToList    O(n^2)   |
    --------------------------------

    -}


-- EJERCICIO 3.1:

    -- Implementado en Multiset.hs


-- EJERCICIO 3.2:

ocurrencias' :: String -> MultiSet Char
    -- COSTO: O().
    -- Siendo 
ocurrencias' []     = emptyMS
ocurrencias' (c:cs) = addMS c (ocurrencias' cs) 