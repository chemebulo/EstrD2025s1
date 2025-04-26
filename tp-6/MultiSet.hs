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
- addMS             O()
- ocurrencesMS      O()
- unionMS           O()
- intersectionMS    O()
- multiSetToList    O()

-}

------------------------------------------------ IMPLEMENTACIÓN ------------------------------------------------ 

emptyMS :: MultiSet a
-- PROP: denota un multiconjunto vacío.
emptyMS = MS emptyM


addMS :: Ord a => a -> MultiSet a -> MultiSet a
-- PROP: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al multiconjunto.
addMS x (MS m) = undefined


ocurrencesMS :: Ord a => a -> MultiSet a -> Int
-- PROP: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese elemento en el multiconjunto.
ocurrencesMS x (MS m) = undefined


unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a  -- (opcional)
-- PROP: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de ambos multiconjuntos.
unionMS (MS m1) (MS m2) = MS (unionEntre (keys m1) m1 m2)

unionEntre :: Eq k => [k] -> Map a Int -> Map a Int -> Map a Int
unionEntre ks m1 m2 = undefined


intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a -- (opcional)
-- PROP: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos multiconjuntos tienen en común.
intersectionMS (MS m1) (MS m2) = MS (interseccionEntre (keys m1) m1 m2)

interseccionEntre ::[k] -> Map a Int -> Map a Int -> Map a Int
interseccionEntre ks m1 m2 = undefined


multiSetToList :: MultiSet a -> [(a, Int)]
-- PROP: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y su cantidad de o currencias.
multiSetToList (MS m) = mapToList (keys m) m

mapToList :: [k] -> Map a Int -> [(a, Int)]
mapToList []     m = []
mapToList (k:ks) m = let n = fromJust (lookupM k m)
                     in (k, n) : mapToList ks m