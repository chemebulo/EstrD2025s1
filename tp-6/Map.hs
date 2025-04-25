module Map
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where 

data Map k v = M [(k, v)]
    deriving Show
{- INV. REP.:
    * Sea M [(k, v)]: cada cada par (k,v) representa un par de clave-valor. La lista no puede tener claves repetidas.
-}

{- COSTO OPERACIONAL DE CADA FUNCIÓN:

- emptyM    O(1)
- assocM    O(n)
- lookupM   O(n)
- deleteM   O(n)
- keys      O(n)

-}

------------------------------------------------ IMPLEMENTACIÓN ------------------------------------------------ 

emptyM :: Map k v
-- PROP: Devuelve un map vacío.
emptyM = M []


assocM :: Eq k => k -> v -> Map k v -> Map k v
-- PROP: Agrega una asociación clave-valor al map.
assocM x y (M kvs) = M (asociarEn x y kvs)

asociarEn :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
asociarEn x y []          = [(x, y)]
asociarEn x y ((k,v):kvs) = if x == k 
                               then (x, y):kvs
                               else (k, v):asociarEn x y kvs


lookupM :: Eq k => k -> Map k v -> Maybe v
-- PROP: Encuentra un valor dado una clave.
lookupM x (M kvs) = buscarValorDeEn x kvs 

buscarValorDeEn :: Eq k => k -> [(k, v)] -> Maybe v
buscarValorDeEn x []          = Nothing
buscarValorDeEn x ((k,v):kvs) = if x == k
                                   then Just v
                                   else buscarValorDeEn x kvs 


deleteM :: Eq k => k -> Map k v -> Map k v
-- PROP: Borra una asociación dada una clave.
deleteM k (M kvs) = M (borrarAsociacionEn k kvs)

borrarAsociacionEn :: Eq k => k -> [(k, v)] -> [(k, v)]
borrarAsociacionEn x []          = []
borrarAsociacionEn x ((k,v):kvs) = if x == k
                                     then kvs
                                     else (k,v):borrarAsociacionEn x kvs

keys :: Map k v -> [k]
-- PROP: Devuelve las claves del map.
keys (M kvs) = clavesEn kvs

clavesEn :: [(k, v)] -> [k]
clavesEn []          = []
clavesEn ((k,v):kvs) = k : clavesEn kvs