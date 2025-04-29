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

-- #################################################### IMPLEMENTACIÓN ####################################################

emptyM :: Map k v
-- PROP: Devuelve un map vacío.
    -- COSTO: O(1).
    -- Siendo en el peor caso de costo constante por el hecho de solamente utilizar el constructor con una lista vacía. 
emptyM = M []


assocM :: Eq k => k -> v -> Map k v -> Map k v
-- PROP: Agrega una asociación clave-valor al map.
    -- COSTO: O(1).
    -- Siendo kvs la lista de pares clave-valor dada, k la clave dada y v el valor dado, se utiliza la operación "cons" en kvs
    -- de costo constante en el peor caso. 
assocM k v (M kvs) = M ((k,v):kvs)


lookupM :: Eq k => k -> Map k v -> Maybe v
-- PROP: Encuentra un valor dado una clave.
    -- COSTO: O(n).
    -- Siendo kvs la lista de pares clave-valor dada, se realiza una operación lineal sobre la misma. En el peor caso,
    -- (kvs sin la clave o está al final) termina encontrando el valor de la clave dada al final de la lista, resultando 
    -- en que "lookupM" resulta de costo lineal.
lookupM k (M kvs) = encontrarEn k kvs


deleteM :: Eq k => k -> Map k v -> Map k v
-- PROP: Borra una asociación dada una clave.
    -- COSTO: O(n).
    -- Siendo kvs la lista de pares clave-valor dada, se realiza una operación lineal sobre la misma. En el peor caso,
    -- aunque en realidad siempre pasa, llega al final de la lista borrando todas las claves que coincidan con "k".
    -- Esto resulta en que "deleteM" termine siendo de costo lineal.
deleteM k (M kvs) = M (eliminarEn k kvs)


keys :: Eq k => Map k v -> [k]
-- PROP: Devuelve las claves del map.
    -- COSTO: O(n^2).
    -- Siendo kvs la lista de pares clave-valor dada, se realiza una operacion de costo 'n^2' sobre la misma. En el 
    -- peor caso (aunque asegurado que termina haciendo eso), agrega todas las claves de kvs en una lista, resultando
    -- que "keys" termine siendo de costo 'n^2'.
keys (M kvs) = soloKeys kvs

-- #################################################### AUXILIARES ####################################################

encontrarEn :: Eq k => k -> [(k, v)] -> Maybe v
    -- COSTO: O(n).
    -- Siendo kvs la lista de pares clave-valor dada, se realiza una operación constante entre la clave dada "nk" y cada
    -- clave de kvs, resultando de costo lineal ya que el peor caso posible es que termine llegando al final de kvs, ya
    -- sea porque no termine encontrando el valor de la clave dada, o porque está justo al final de kvs.
encontrarEn nk []          = Nothing
encontrarEn nk ((k,v):kvs) = if nk == k
                                then Just v
                                else encontrarEn nk kvs


eliminarEn :: Eq k => k -> [(k,v)] -> [(k,v)]
    -- COSTO: O(n).
    -- Siendo kvs la lista de pares clave-valor dada, se realiza una operación constante entre la clave dada "nk" y cada
    -- clave de kvs, y encima por como está construida, tiene asegurado llegar al final de la lista ya que elimina cada
    -- uno de las claves en kvs que coincidan con "nk". Resultando que "eliminarEn" sea de costo lineal.
eliminarEn nk []          = []
eliminarEn nk ((k,v):kvs) = if nk == k
                               then eliminarEn nk kvs
                               else (k,v) : eliminarEn nk kvs


soloKeys :: Eq k => [(k, v)] -> [k]
    -- COSTO: O(n^2).
    -- Siendo n la cantidad de elementos de la lista de pares clave-valor dada, se realiza una operación de costo lineal n
    -- para cada n. En el peor caso (aunque asegurado que va a hacer eso), resulta que el costo de "soloKeys" sea de 'n^2'.
soloKeys []          = []
soloKeys ((k,v):kvs) = if not (estaEnPares k kvs)
                          then k : soloKeys kvs
                          else soloKeys kvs

estaEnPares :: Eq k => k -> [(k, v)] -> Bool
    -- COSTO: O(n).
    -- Siendo kvs la lista de pares clave-valor dada, se realiza una operación constante para cada clave de kvs. En el
    -- peor caso termina llegando al final de kvs preguntando si "nk" es la clave del par de ese momento. Esto resulta
    -- en que "estaEnPares" sea de costo lineal.
estaEnPares nk []          = False
estaEnPares nk ((k,v):kvs) = nk == k || estaEnPares nk kvs