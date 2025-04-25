module MapV3
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where

data Map k v = M [k] [v]
    deriving Show
{- INV. REP.:
    * Sea M ks vs: no pueden haber keys repetidas en ks.
    * Sea M ks vs: las listas ks y vs deben tener la misma longitud.
-}

{- COSTO OPERACIONAL DE CADA FUNCIÓN:

- emptyM    O(1)
- assocM    O(n)
- lookupM   O(n)
- deleteM   O(n)
- keys      O(1)

-}

------------------------------------------------ IMPLEMENTACIÓN ------------------------------------------------ 

emptyM :: Map k v
-- PROP: Devuelve un map vacío
emptyM = M [] []


assocM :: Eq k => k -> v -> Map k v -> Map k v
-- PROP: Agrega una asociación clave-valor al map.
assocM k v (M ks1 vs1) = let (ks, vs) = agregarEn k v ks1 vs1
                         in M ks vs

agregarEn :: Eq k => k -> v -> [k] -> [v] -> ([k],[v])
-- PRECOND: Las listas dadas deben tener la misma longitud.
agregarEn nk nv []     _      = ([nk], [nv])
agregarEn nk nv (k:ks) (v:vs) = if nk == k
                                   then (nk:ks, nv:vs)
                                   else agregar nk nv (agregarEn nk nv ks vs)

agregar :: k -> v -> ([k], [v]) -> ([k], [v])
agregar nk nv (ks, vs) = (nk:ks, nv:vs)


lookupM :: Eq k => k -> Map k v -> Maybe v
-- PROP: Encuentra un valor dado una clave.
lookupM k (M ks vs) = valueEn k ks vs

valueEn :: Eq k => k -> [k] -> [v] -> Maybe v
-- PRECOND: Las listas dadas deben tener la misma longitud.
valueEn nk []     _      = Nothing
valueEn nk (k:ks) (v:vs) = if nk == k
                              then Just v
                              else valueEn nk ks vs  


deleteM :: Eq k => k -> Map k v -> Map k v
-- PROP: Borra una asociación dada una clave.
deleteM k (M xs ys) = let (ks, vs) = eliminarM k ks vs
                      in M ks vs

eliminarM :: Eq k => k -> [k] -> [v] -> ([k],[v])
-- PRECOND: Las listas dadas deben tener la misma longitud.
eliminarM nk [] _          = ([], [])
eliminarM nk (k:ks) (v:vs) = if nk == k
                                then (ks, vs)
                                else agregar k v (eliminarM nk ks vs)


keys :: Map k v -> [k]
-- PROP: Devuelve las claves del map.
keys (M ks vs) = ks