module MapV2
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where 

data Map k v = M [(k, v)]
    deriving Show
{- INV. REP.:
    * Sea M [(k, v)]: cada k (clave) se asocia a un v (valor). Admite claves repetidas.
-}

{- COSTO OPERACIONAL DE CADA FUNCIÓN:

- emptyM    O()
- assocM    O()
- lookupM   O()
- deleteM   O()
- keys      O()

-}

------------------------------------------------ IMPLEMENTACIÓN ------------------------------------------------ 

emptyM :: Map k v
-- PROP: Devuelve un map vacío
emptyM = undefined


assocM :: Eq k => k -> v -> Map k v -> Map k v
-- PROP: Agrega una asociación clave-valor al map.
assocM = undefined


lookupM :: Eq k => k -> Map k v -> Maybe v
-- PROP: Encuentra un valor dado una clave.
lookupM = undefined


deleteM :: Eq k => k -> Map k v -> Map k v
-- PROP: Borra una asociación dada una clave.
deleteM = undefined


keys :: Map k v -> [k]
-- PROP: Devuelve las claves del map.
keys = undefined