module MultiSet
    (MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList)
where

import Map
-- Todas las implementaciones de Multiset estan basadas en la versión 1 de Map.

data MultiSet a = MS (Map a Int)
    deriving Show
{- INV. REP.:
    * Ninguna.
-}

{- COSTO OPERACIONAL DE CADA FUNCIÓN:

- emptyMS           O(1)
- addMS             O(n)
- ocurrencesMS      O(n)
- unionMS           O(n^2)
- intersectionMS    O(n^2)
- multiSetToList    O(n^2)

-}

-- #################################################### IMPLEMENTACIÓN ####################################################

emptyMS :: MultiSet a
-- PROP: Denota un multiconjunto vacío.
    -- COSTO: O(1).
    -- Siendo en el peor caso de costo constante por el hecho de solamente utilizar el constructor con un map vacío (de 
    -- costo constante también).
emptyMS = MS emptyM


addMS :: Ord a => a -> MultiSet a -> MultiSet a
-- PROP: Dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al multiconjunto.
    -- COSTO: O(n).
    -- Siendo n la clave dada, y m el Map del Multiset dado; por cada n se utilizan varias funciones de costo lineal (lookupM y assocM) y además,
    -- se utiliza la función "elem" de costo lineal, ya que en el peor caso llega al final de ns para encontrar a n. Esto resulta que el costo
    -- total de la función es lineal, ya que por cada n realizan varias operaciones de costo lineal => (n + n = n).   
addMS k (MS m) = MS (addMS' k (keys m) m)


ocurrencesMS :: Ord a => a -> MultiSet a -> Int
-- PROP: Dados un elemento y un multiconjunto indica la cantidad de apariciones de ese elemento en el multiconjunto.
    -- COSTO: O(n).
    -- Siendo n la clave dada, y m el Map del Multiset dado; se utiliza la función "ocurrencesMS'" de costo lineal, esto termina resultando
    -- con que el costo total de la función sea de dicho costo, siendo n lo que tardarían la operación lookupM y la función "elem", ya que
    -- ambas tienen costo lineal en el peor caso (n + n => n).  
ocurrencesMS k (MS m) = ocurrencesMS' k (keys m) m


unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
-- PROP: Dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de ambos multiconjuntos.
    -- COSTO: O(n^2).
    -- Siendo m1 y m2, Maps de los Multisets dados; se utiliza el constructor (de costo constante), y dentro la función "unionEntre" de costo
    -- cuadrático en el peor caso, ya que por cada n del m1, se realizan n operaciones ("lookupM" y "assocM" (lineales)) y además por cada
    -- n de m2 también.
unionMS (MS m1) (MS m2) = MS (unionEntre (keys m1) m1 m2)


intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
-- PROP: Dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos multiconjuntos tienen en común.
    -- COSTO: O(n^2).
    -- Siendo m1 y m2, Maps de los Multisets dados; se utiliza el constructor (de costo constante), y dentro la función "interseccionEntre" de
    -- cuadrático en el peor caso, ya que por cada n del m1, se realizan n operaciones ("lookupM" y "assocM" (lineales)) y además por cada
    -- n de m2 también.
intersectionMS (MS m1) (MS m2) = MS (interseccionEntre (keys m1) m1 m2)


multiSetToList :: Eq a => MultiSet a -> [(a, Int)]
-- PROP: Dado un multiconjunto devuelve una lista con todos los elementos del conjunto y su cantidad de ocurrencias.
    -- COSTO: O(n^2).
    -- Siendo m el Map del Multiset dado y n la cantidad de claves de m; se utiliza la función "mapToList" de costo cuadrático en el peor caso, 
    -- es por eso que el costo total de la función termina siendo cuadrática.
multiSetToList (MS m) = mapToList (keys m) m

-- #################################################### AUXILIARES ####################################################

addMS' :: Ord a => a -> [a] -> Map a Int -> Map a Int
    -- COSTO: O(n).
    -- Siendo n la clave dada, y ns la lista de claves dada; por cada n se utilizan varias funciones de costo lineal (lookupM y assocM) y además,
    -- se utiliza la función "elem" de costo lineal, ya que en el peor caso llega al final de ns para encontrar a n. Esto resulta que el costo
    -- total de la función es lineal, ya que por cada n realizan varias operaciones de costo lineal => (n + n = n).   
addMS' k ks m = let nv = fromJust (lookupM k m) + 1
                in if elem k ks
                      then assocM k nv m
                      else assocM k 1  m

fromJust :: Maybe a -> a
    -- COSTO: O(1).
    -- Siendo x el dato de tipo "maybe a", devuelve x en el caso de que tenga el constructor "Just", entonces, eso implica que el costo
    -- de todo el funcionamiento en el peor caso sea constante, ya que solo devuelve el dato. 
fromJust Nothing   = error "No tendria que dar esto."
fromJust (Just x)  = x


ocurrencesMS' :: Ord a => a -> [a] -> Map a Int -> Int
    -- COSTO: O(n).
    -- Siendo n la clave dada, y m el Map del Multiset dado; se utiliza la función "ocurrencesMS'" de costo lineal, esto termina resultando
    -- con que el costo total de la función sea de dicho costo, siendo n lo que tardarían la operación lookupM y la función "elem", ya que
    -- ambas tienen costo lineal en el peor caso (n + n => n).  
ocurrencesMS' k ks m = let v = fromJust (lookupM k m)
                       in if elem k ks
                             then v
                             else 0


unionEntre :: Eq a => [a] -> Map a Int -> Map a Int -> Map a Int
    -- COSTO: O(n^2).
    -- Siendo n un elemento de la lista de claves dada, por cada n se realizan varias operaciones lineales (como "lookupM"), y además, se utiliza
    -- la función "notElem" (de costo lineal en el peor caso). Esto resulta con que el costo total de la función se cuadrático, ya que por cada n
    -- se realizan n operaciones.
unionEntre []     m1 m2 = m2
unionEntre (k:ks) m1 m2 = let vm1 = fromJust (lookupM k m1); vm2 = fromJust (lookupM k m2)
                          in if notElem k (keys m2)
                                then unionEntre ks m1 (assocM k vm1 m2)
                                else unionEntre ks m1 (assocM k (vm1 + vm2) m2)


interseccionEntre :: Ord a => [a] -> Map a Int -> Map a Int -> Map a Int
    -- COSTO: O(n^2).
    -- Siendo n un elemento de la lista de claves dada, por cada n se realizan varias operaciones lineales (como "lookupM"), y además, se utiliza
    -- la función "notElem" (de costo lineal en el peor caso). Esto resulta con que el costo total de la función se cuadrático, ya que por cada n
    -- se realizan n operaciones.
interseccionEntre []     m1 m2 = emptyM
interseccionEntre (k:ks) m1 m2 = let vm1 = fromJust (lookupM k m1); vm2 = fromJust (lookupM k m2)
                                 in if notElem k (keys m2)
                                       then interseccionEntre ks m1 m2
                                       else assocM k (min vm1 vm2) (interseccionEntre ks m1 m2)


mapToList :: Eq a => [a] -> Map a Int -> [(a, Int)]
    -- COSTO: O(n^2).
    -- Siendo n un elemento de la lista de claves dada, por cada n se utiliza la función "fromJust" de costo constante, aunque utiliza como
    -- argumento la operación "lookupM" (de costo lineal en el peor caso). Esto termina resultando que el costo total de la función es cuadrático,
    -- ya que por cada n se realizan n operaciones en el peor caso.
mapToList []     m = []
mapToList (k:ks) m = let n = fromJust (lookupM k m)
                     in (k, n) : mapToList ks m