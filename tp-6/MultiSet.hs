module MultiSet
    (MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList)
where

import Map

data MultiSet a = MS (Map a Int)
    deriving Show
{- INV. REP.:
    * Ninguna.
-}

{- COSTO OPERACIONAL DE CADA FUNCIÓN:

- emptyMS           O(1)
- addMS             O(n)
- ocurrencesMS      O(n)
- unionMS           O()
- intersectionMS    O()
- multiSetToList    O()

-}

------------------------------------------------ IMPLEMENTACIÓN ------------------------------------------------ 

emptyMS :: MultiSet a
-- PROP: Denota un multiconjunto vacío.
emptyMS = MS emptyM


addMS :: Ord a => a -> MultiSet a -> MultiSet a
-- PROP: Dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al multiconjunto.
addMS k (MS m) = MS (addMS' k (keys m) m)

addMS' :: Ord a => a -> [a] -> Map a Int -> Map a Int
addMS' k ks m = let nv = fromJust (lookupM k m) + 1
                in if elem k ks
                      then assocM k nv m
                      else assocM k 1  m

fromJust :: Maybe a -> a
fromJust Nothing   = error "No tendria que dar esto."
fromJust (Just x)  = x


ocurrencesMS :: Ord a => a -> MultiSet a -> Int
-- PROP: Dados un elemento y un multiconjunto indica la cantidad de apariciones de ese elemento en el multiconjunto.
ocurrencesMS k (MS m) = ocurrencesMS' k (keys m) m

ocurrencesMS' :: Ord a => a -> [a] -> Map a Int -> Int
ocurrencesMS' k ks m = let v = fromJust (lookupM k m)
                       in if elem k ks
                             then v
                             else 0


unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
-- PROP: Dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de ambos multiconjuntos.
unionMS (MS m1) (MS m2) = MS (unionEntre (keys m1) m1 m2)

unionEntre :: Eq a => [a] -> Map a Int -> Map a Int -> Map a Int
unionEntre []     m1 m2 = m2
unionEntre (k:ks) m1 m2 = let vm1 = fromJust (lookupM k m1); vm2 = fromJust (lookupM k m2)
                          in if notElem k (keys m2)
                                then unionEntre ks m1 (assocM k vm1 m2)
                                else unionEntre ks m1 (assocM k (vm1 + vm2) m2)


intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
-- PROP: Dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos multiconjuntos tienen en común.
intersectionMS (MS m1) (MS m2) = MS (interseccionEntre (keys m1) m1 m2)

interseccionEntre :: Ord a => [a] -> Map a Int -> Map a Int -> Map a Int
interseccionEntre []     m1 m2 = emptyM
interseccionEntre (k:ks) m1 m2 = let vm1 = fromJust (lookupM k m1); vm2 = fromJust (lookupM k m2)
                                 in if notElem k (keys m2)
                                       then interseccionEntre ks m1 m2
                                       else assocM k (min vm1 vm2) (interseccionEntre ks m1 m2)


multiSetToList :: Eq a => MultiSet a -> [(a, Int)]
-- PROP: Dado un multiconjunto devuelve una lista con todos los elementos del conjunto y su cantidad de ocurrencias.
multiSetToList (MS m) = mapToList (keys m) m

mapToList :: Eq a => [a] -> Map a Int -> [(a, Int)]
mapToList []     m = []
mapToList (k:ks) m = let n = fromJust (lookupM k m)
                     in (k, n) : mapToList ks m