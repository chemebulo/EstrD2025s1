-- EJERCICIO 1:

-- El costo de heapsort :: Ord a => [a] -> [a] es 'n log n', ya que se toma el peor caso de eficiencia de las funciones utilizadas.

    {- 
        heapSort :: Ord a => [a] -> [a]
        -- PROP: Dada una lista, la ordena de menor a mayor utilizando una Priority Queue como estructura auxiliar.
            -- COSTO: O(n log n) que es lo mismo que (n*2 log n).
            -- Siendo n la cantidad de elementos de la lista dada, se utiliza en toda la lista "listaAPriorityQueue" de costo 'n log n',
            -- y para la priority queue resultante "priorityQueueALista" de costo 'n log n'.     
        heapSort xs = priorityQueueALista (listaAPriorityQueue xs)
        
        
        priorityQueueALista :: Ord a => PriorityQueue a -> [a]
            -- COSTO: O(n log n).
            -- Siendo n la cantidad de elementos de la priority queue, para cada elemento se utiliza "findMinPQ" de costo costante y se elimina 
            -- el mínimo de la lista dada utilizando "deleteMinPQ" de costo 'log n'; y también se utiliza "isEmptyPQ" de costo constante. 
        priorityQueueALista pq = if not (isEmptyPQ pq)
                                    then findMinPQ pq : priorityQueueALista (deleteMinPQ pq)
                                    else []
        
        
        listaAPriorityQueue :: Ord a => [a] -> PriorityQueue a
            -- COSTO: O(n log n). 
            -- Siendo n la cantidad de elementos de la lista dada, se utiliza "insertPQ" de costo 'log n' en cada uno de ellos.  
        listaAPriorityQueue []     = emptyPQ
        listaAPriorityQueue (x:xs) = insertPQ x (listaAPriorityQueue xs)
    -}


-- EJERCICIO 2:

---------------------------- AUXILIARES ----------------------------
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show
--------------------------------------------------------------------
inorderT :: Tree a -> [a]
inorderT EmptyT          = []
inorderT (NodeT x ti td) = inorderT ti ++ [x] ++ inorderT td
--------------------------------------------------------------------


belongsBST :: Ord a => a -> Tree a -> Bool
-- PROP: Dado un BST dice si el elemento pertenece o no al árbol.
-- PRECOND: El árbol dado cumple los invariantes de BST y no tiene elementos repetidos.
    -- COSTO: O(log n).
    -- Siendo n la cantidad de elementos del árbol dado, en cada paso de la búsqueda se realiza una comparación de costo constante
    -- y se descarta la mitad del árbol. Por lo tanto, se recorren a lo sumo 'log n' niveles del árbol, resultando en un costo total
    -- logarítmico en la cantidad de elementos.
belongsBST x EmptyT          = False
belongsBST x (NodeT y ti td) = if x == y then True 
                          else if x < y  then belongsBST x ti 
                                         else belongsBST x td


insertBST :: Ord a => a -> Tree a -> Tree a
-- PROP: Dado un BST inserta un elemento en el árbol.
-- PRECOND: El árbol dado cumple los invariantes de BST y no tiene elementos repetidos.
    -- COSTO: O(log n).
    -- Siendo n la cantidad de elementos del árbol dado, en cada paso de la búsqueda se realiza una comparación de costo constante
    -- y se descarta la mitad del árbol. Por lo tanto, se recorren a lo sumo 'log n' niveles del árbol, resultando en un costo total
    -- logarítmico en la cantidad de elementos.
insertBST x EmptyT          = NodeT x EmptyT EmptyT
insertBST x (NodeT y ti td) = if x == y then NodeT x ti td 
                         else if x < y  then NodeT y (insertBST x ti) td
                                        else NodeT y ti (insertBST x td)


deleteBST :: Ord a => a -> Tree a -> Tree a
-- PROP: Dado un BST borra un elemento en el árbol.
-- PRECOND: El árbol dado cumple los invariantes de BST y no tiene elementos repetidos.
    -- COSTO: O(log n).
    -- Siendo n la cantidad de elementos del árbol dado...
deleteBST x EmtpyT          = EmptyT
deleteBST x (NodeT y ti td) = if x == y then rearmarBST ti td
                         else if x < y  then NodeT y (deleteBST x ti) td
                                        else NodeT y ti (deleteBST x td)

rearmarBST :: Od a => Tree a -> Tree a -> Tree a
-- PRECOND: Los árboles dados cumplen los invariantes de BST y no tienen elementos repetidos.
    -- COSTO: O(log n).
    -- Siendo n la cantidad de elementos del árbol dado...
rearmarBST EmptyT td = td
rearmarBST ti     td = let (m, ti') = splitMaxBST ti
                        in NodeT m ti' td


splitMinBST :: Ord a => Tree a -> (a, Tree a)
-- PROP: Dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
-- PRECOND: El árbol dado cumple los invariantes de BST, no tiene elementos repetidos, y no está vacío.
    -- COSTO: O(log n).
    -- Siendo n la cantidad de elementos del árbol dado...
splitMinBST (NodeT x EmptyT ti) = (x, td)
splitMinBST (NodeT x ti     td) = let (m, ti') = splitMaxBST ti
                                   in (m, NodeT x ti' td)


splitMaxBST :: Ord a => Tree a -> (a, Tree a)
-- PROP: Dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
-- PRECOND: El árbol dado cumple los invariantes de BST, no tiene elementos repetidos, y no está vacío.
    -- COSTO: O(log n).
    -- Siendo n la cantidad de elementos del árbol dado...
splitMaxBST (NodeT x ti EmptyT) = (x, ti)
splitMaxBST (NodeT x ti td)     = let (m, td') = splitMaxBST td
                                   in (m, NodeT x ti td')


esBST :: Tree a -> Bool
-- PROP: Indica si el árbol cumple con los invariantes de BST.
-- PRECOND: El árbol dado cumple los invariantes de BST y no tiene elementos repetidos.
    -- COSTO: O(n^2).
    -- Siendo n la cantidad de elementos del árbol dado...
esBST EmptyT          = undefined
esBST (NodeT x ti td) = undefined


elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
-- PROP: Dado un BST y un elemento, devuelve el máximo elemento que sea menor al elemento dado.
-- PRECOND: El árbol dado cumple los invariantes de BST y no tiene elementos repetidos.
    -- COSTO: O(log n).
    -- Siendo n la cantidad de elementos del árbol dado...
elMaximoMenorA = undefined


elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
-- PROP: Dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al elemento dado.
-- PRECOND: El árbol dado cumple los invariantes de BST y no tiene elementos repetidos.
    -- COSTO: O(log n).
    -- Siendo n la cantidad de elementos del árbol dado...
elMinimoMayorA = undefined


balanceado :: Tree a -> Bool
-- PROP: Indica si el árbol está balanceado. Un árbol está balanceado cuando para cada nodo la diferencia de
--       alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
-- PRECOND: Cumple los invariantes de BST y no tiene elementos repetidos.
    -- COSTO: O(n^2).
    -- Siendo n la cantidad de elementos del árbol dado...
balanceado EmptyT = True
balanceado (NodeT _ ti td) = abs (altura ti - altura td) <= 1 && balanceado ti && balanceado td

altura :: Tree a -> Int
altura EmptyT = 0
altura (NodeT _ ti td) = 1 + max (altura ti) (altura td)
-- REVISARRRRRRRRRRRRRRRRRRRR



-- EJERCICIO 3:

{- 
    ---------------------------------------------------------
    |        BASADO EN LA SIGUIENTE INTERFAZ DE MAP         |
    |-------------------------------------------------------|
    |  emptyM :: Map k v                                    |
    |  Costo: O(1).                                         |
    |-------------------------------------------------------|
    |  assocM :: Ord k => k -> v -> Map k v -> Map k v      |   
    |  Costo: O(log K).                                     |   
    |-------------------------------------------------------|
    |  lookupM :: Ord k => k -> Map k v -> Maybe v          |
    |  Costo: O(log K).                                     |
    |-------------------------------------------------------|
    |  deleteM :: Ord k => k -> Map k v -> Map k v          |
    |  Costo: O(log K).                                     |
    |-------------------------------------------------------|
    |  keys :: Map k v -> [k]                               |
    |  Costo: O(K)                                          |
    ---------------------------------------------------------
-}

valuesM :: Eq k => Map k v -> [Maybe v]
-- PROP: Obtiene los valores asociados a cada clave del map.
    -- COSTO: O(n*m).
    -- Siendo n la cantidad de claves en el map, se obtienen las claves del map utilizando la operación "keys" de
    -- costo lineal. Posteriormente se obtienen los valores de cada clave con "valoresDeKeysEn" sobre en que también
    -- es lineal por cada lookupM. Esto resulta en que el costo total es 'n*m'.
valuesM m = valoresDeKeysEn (keys m) m

valoresDeKeysEn :: Eq k => [k] -> Map k v -> [Maybe v]
    -- COSTO: O(n*m).
    -- Siendo m la longitud de la lista de claves dada y n la cantidad de claves en el map, se utiliza la operación
    -- "lookupM" de costo lineal por cada clave de la lista. Esto termina resultando que el costo total sea 'n*m'.
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
    -- son operaciones lineales (ya que elem es una búsqueda lineal sobre la lista), lo que termina resultando que el costo total sea lineal.
estaAsociadoEn k m = elem k (keys m)


listToMap :: Eq k => [(k, v)] -> Map k v
-- PROP: Convierte una lista de pares clave valor en un map.
    -- COSTO: O(n^2).
    -- Siendo n cada par de la lista de pares dada (kvs), se realiza la operación "assocM" en cada uno de ellos. Esto resulta que por 
    -- cada n se realiza una operación de costo lineal en el peor caso, por ende, el costo total de la función es cuadrático.    
listToMap []          = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)


mapToList :: Eq k => Map k v -> [(k, v)]
-- PROP: Convierte un map en una lista de pares clave valor.
    -- COSTO: O(n^2).
    -- Siendo m el mapa dado, y n las claves de dicho mapa; se utiliza la función "mapALista" de costo cuadrático, resultando entonces,
    -- que el costo total de la función es cuadrático. 
mapToList m = mapALista m (keys m)

mapALista :: Eq k => Map k v -> [k] -> [(k, v)]
    -- COSTO: O(n^2).
    -- Siendo m el mapa dado, y ks las claves dadas; para cada clave de ks se utiliza la operación "lookupM" de costo lineal en el peor caso
    -- como argumento de la función "fromJust" (lo cual sabiendo que las claves dadas son claves de m, asegura que hay valores para cada una de ellas), 
    -- además que va utilizando "cons" de costo constante para agregar cada par a la lista resultante de ks. Esto implica, entonces, que el costo total 
    -- de la función es cuadrático. 
mapALista m []     = []
mapALista m (k:ks) = let v = fromJust (lookupM k m)
                     in (k, v) : mapALista m ks

fromJust :: Maybe a -> a
    -- COSTO: O(1).
    -- Siendo x el dato de tipo "maybe a", devuelve x en el caso de que tenga el constructor "Just", entonces, eso implica que el costo
    -- de todo el funcionamiento en el peor caso sea constante, ya que solo devuelve el dato. 
fromJust Nothing   = error "No tendria que dar esto."
fromJust (Just x)  = x


agruparEq :: Eq k => [(k, v)] -> Map k [v]
-- PROP: Dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave.
    -- COSTO: O(n^2).
    -- Siendo kvs la lista de pares clave-valor dada, se utiliza la función "agruparValuesAKeys" de costo cuadrático sobre las claves de kvs
    -- y sobre kvs. Esto termina resultando que el costo total de la función sea cuadrático, ya que termina costando lo que "agruparValuesAKeys" cueste. 
agruparEq kvs = agruparValuesAKeys (soloKeys kvs) kvs

agruparValuesAKeys :: Eq k => [k] -> [(k, v)] -> Map k [v]
    -- COSTO: O(n^2).
    -- Siendo n la cantidad de claves de la lista dada, por cada n se realiza la operación "assocM" de costo lineal, además que también
    -- se utiliza la función "agruparValuesDeKey" de costo lineal. Es por eso, que el costo total termina resultando de costo cuadrático,
    -- ya que por cada n (cada clave) se realiza n operaciones. 
agruparValuesAKeys []     kvs = emptyM
agruparValuesAKeys (k:ks) kvs = assocM k (agruparValuesDeKey k kvs) (agruparValuesAKeys ks kvs)

agruparValuesDeKey :: Eq k => k -> [(k, v)] -> [v]
    -- COSTO: O(n).
    -- Siendo n la cantidad de pares de la lista dada (kvs), por cada n se realiza una operación constante de comparación, y en el peor caso,
    -- se termina llegando al final de kvs. Es por eso, que el costo total de la función en el peor caso es lineal, ya que depende de n. 
agruparValuesDeKey nk []          = []
agruparValuesDeKey nk ((k,v):kvs) = if nk == k
                                       then v : agruparValuesDeKey nk kvs
                                       else agruparValuesDeKey nk kvs

soloKeys :: Eq k => [(k, v)] -> [k]
    -- COSTO: O(n^2).
    -- Siendo kvs la lista de pares dado, para cada clave de kvs se utiliza la función "estaEnPares" de costo lineal en el peor caso,
    -- resultando entonces, que en el peor caso, que el costo total de la función sea cuadrático (la utiliza en cada par de kvs y por diseño,
    -- va a recorrer toda la lista).
soloKeys []          = []
soloKeys ((k,v):kvs) = if not (estaEnPares k kvs)
                          then k : soloKeys kvs
                          else soloKeys kvs

estaEnPares :: Eq k => k -> [(k, v)] -> Bool
    -- COSTO: O(n).
    -- Siendo kvs la lista de pares dado, y nk la clave dada; para cada par de kvs se realiza la operación de comparación de costo constante,
    -- resultando entonces, que en el peor caso, la comparación se realiza en todos los elementos de kvs, lo cual termina explicando que
    -- el costo total de la función es lineal.
estaEnPares nk []          = False
estaEnPares nk ((k,v):kvs) = nk == k || estaEnPares nk kvs


incrementar :: Eq k => [k] -> Map k Int -> Map k Int
-- PROP: Dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a cada número asociado con dichas claves.
    -- COSTO: O(n^2).
    -- Siendo ks la lista de claves dada, y m el mapa dado; para cada clave de ks se utiliza la función "fromJust" con la operación "lookupM"
    -- como argumento (el cual tiene costo lineal), y también la función "estaAsociadoEn" (de costo lineal). En el caso de que la clave de ks
    -- dada esté asocidada, se realiza la operación "assocM" (de costo lineal). Todo esto termina resultando en que por cada clave de ks se 
    -- realizan operaciones lineales, dando entonces con que el costo total de la función en el peor caso es cuadrático. 
incrementar []     m = m
incrementar (k:ks) m = let v = fromJust (lookupM k m)
                       in if estaAsociadoEn k m
                             then assocM k (v + 1) (incrementar ks m)
                             else incrementar ks m


mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
-- PROP: Dado dos maps se agregan las claves y valores del primer map en el segundo. Si una clave del primero existe en el segundo,
--       es reemplazada por la del primero.
    -- COSTO: O(n*m + n).
    -- Siendo m1 y m2 los mapas dados, se utiliza la función "agregarMapAMap" de costo 'n*m' y la operación "keys" de costo lineal en el peor caso,
    -- el resultado del costo total de la función es 'n*m + n'.
mergeMaps m1 m2 = agregarMapAMap (keys m1) m1 m2

agregarMapAMap :: Eq k => [k] -> Map k v -> Map k v -> Map k v
    -- COSTO: O(n*m).
    -- Siendo ks la lista de claves dada (y n cada clave), m el costo lookupM, a el costo de assocM; por cada clave de ks se utiliza la función "fromJust" de 
    -- costo constante, aunque de argumento tiene la operación "lookupM" (de costo lineal), y en la recursión se utiliza un "assocM" (de costo lineal). 
    -- Esto termina resultando con que el costo total de la función en el peor caso es n*m.
agregarMapAMap []     m1 m2 = m2
agregarMapAMap (k:ks) m1 m2 = let v = fromJust (lookupM k m1)
                              in agregarMapAMap ks m1 (assocM k v m2)


indexar :: [a] -> Map Int a
-- PROP: Dada una lista de elementos construye un map que relaciona cada elemento con su posición en la lista.
    -- COSTO: O(n^2).
    -- Siendo xs la lista de elementos dada, se utiliza la función "indexarXS" de costo cuadrático. Esto termina resultando con que el costo
    -- total de la función en el peor caso sea 'n^2', siendo n la cantidad elementos en la lista y n la operación "assocM" dentro de "indexarXS".
indexar xs = indexarXS 1 xs

indexarXS :: Int -> [a] -> Map Int a
    -- COSTO: O(n^2).
    -- Siendo n la cantidad de elementos en xs, por cada n se realiza la operación "assocM" de costo lineal n, lo cual termina resultando
    -- con que el costo total de la función sea 'n^2'.
indexarXS n []     = emptyM
indexarXS n (x:xs) = assocM n x (indexarXS (n+1) xs)


ocurrencias :: String -> Map Char Int
-- PROP: Dado un string, devuelve un map donde las claves son los caracteres que aparecen en el string, y los 
--       valores la cantidad de veces que aparecen en el mismo.
    -- COSTO: O(n^2).
    -- Siendo s el string dado, se utiliza la función "ocurrenciasDe" de costo 'n^2', y como argumento se utiliza "sinRepeticiones" de costo
    -- 'n^2'. Esto resulta en que el costo total de la función sea 'n^2', ya que (n^2 + n^2) => n^2.
ocurrencias s = ocurrenciasDe (sinRepeticiones s) s

ocurrenciasDe :: String -> String -> Map Char Int
    -- COSTO: O(n^2).
    -- Siendo n la cantidad de elementos de cs, por cada n se utiliza la función "aparicionesDeEn" de costo lineal, además de esto, también
    -- se realiza la operación de "assocM" (de costo lineal). Entonces, termina resultando con que el costo total de la función es 'n (n + n)',
    -- ya que por cada n se realizan dos operaciones de costo 'n', lo cual resulta en (n^2 + n^2) => n^2.    
ocurrenciasDe []     s = emptyM
ocurrenciasDe (c:cs) s = let num = aparicionesDeEn c s
                         in assocM c num (ocurrenciasDe cs s)

aparicionesDeEn :: Char -> String -> Int
    -- COSTO: O(n).
    -- Siendo n la cantidad de elementos de cs, por cada n se realiza una operacion constante. Esto resulta en que el costo total de la función
    -- termine siendo lineal, ya que por diseño siempre se termina llegando al final de la lista cs.
aparicionesDeEn x []     = 0
aparicionesDeEn x (c:cs) = if x == c
                              then 1 + aparicionesDeEn x cs
                              else aparicionesDeEn x cs

sinRepeticiones :: Eq a => [a] -> [a]
    -- COSTO: O(n^2).
    -- Siendo n la cantidad de elementos en xs, por cada n se utiliza la funcion "elem" de costo lineal, esto termina resultando con que
    -- el costo total de la función sea 'n^2' (siendo n el costo de "elem", es decir, por cada n se realiza una operación de costo n). 
sinRepeticiones []     = []
sinRepeticiones (x:xs) = if elem x xs
                            then sinRepeticiones xs
                            else x : sinRepeticiones xs


-- EJERCICIO 4:

import Map
import Set
-- 
-- type SectorId = Int
-- 
-- type CUIL = Int
-- 
-- data Empresa = ConsE (Map SectorId (Set Empleado))
-- --                   (Map   CUIL      Empleado)
--     deriving Show


-- EJERCICIO 5:

