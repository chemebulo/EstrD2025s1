module MapV2
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where

data Map k v = M [(k, v)]
    deriving Show
{- INV. REP.:
    * Sea M [(k, v)]: cada cada par (k,v) representa un par de clave-valor. La lista puede tener claves repetidas.
-}

{- COSTO OPERACIONAL DE CADA FUNCIÓN:

- emptyM    O(1)
- assocM    O(1)
- lookupM   O(n)
- deleteM   O(n)
- keys      O(n^2)

-}

------------------------------------------------ IMPLEMENTACIÓN ------------------------------------------------ 

emptyM :: Map k v
-- PROP: Devuelve un map vacío.
emptyM = M []


assocM :: Eq k => k -> v -> Map k v -> Map k v
-- PROP: Agrega una asociación clave-valor al map.
assocM k v (M kvs) = M ((k,v):kvs)


lookupM :: Eq k => k -> Map k v -> Maybe v
-- PROP: Encuentra un valor dado una clave.
lookupM k (M kvs) = encontrarEn k kvs

encontrarEn :: Eq k => k -> [(k, v)] -> Maybe v
encontrarEn nk []          = Nothing
encontrarEn nk ((k,v):kvs) = if nk == k
                                then Just v
                                else encontrarEn nk kvs


deleteM :: Eq k => k -> Map k v -> Map k v
-- PROP: Borra una asociación dada una clave.
deleteM k (M kvs) = M (eliminarEn k kvs)

eliminarEn :: Eq k => k -> [(k,v)] -> [(k,v)]
eliminarEn nk []          = []
eliminarEn nk ((k,v):kvs) = if nk == k
                               then eliminarEn nk kvs
                               else (k,v) : eliminarEn nk kvs


keys :: Eq k => Map k v -> [k]
-- PROP: Devuelve las claves del map.
keys (M kvs) = soloKeys kvs

soloKeys :: Eq k => [(k, v)] -> [k]
soloKeys []          = []
soloKeys ((k,v):kvs) = if not (estaEnPares k kvs)
                          then k : soloKeys kvs
                          else soloKeys kvs

estaEnPares :: Eq k => k -> [(k, v)] -> Bool
estaEnPares nk []          = False
estaEnPares nk ((k,v):kvs) = nk == k || estaEnPares nk kvs