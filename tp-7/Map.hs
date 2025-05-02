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

-- #################################################### IMPLEMENTACIÓN ####################################################

emptyM :: Map k v
-- PROP: Devuelve un map vacío.
    -- COSTO: O(1).
    -- Siendo en el peor caso de costo constante por el hecho de solamente utilizar el constructor con una lista vacía.
emptyM = M []


assocM :: Eq k => k -> v -> Map k v -> Map k v
-- PROP: Agrega una asociación clave-valor al map.
    -- COSTO: O(n).
    -- Siendo kvs la lista de pares clave-valor dada, se realiza una operación lineal sobre la misma. En el peor caso,
    -- (kvs sin la clave) termina insertando el elemento y el valor dado al final de la lista, es por eso que "assocM" 
    -- resulta de costo lineal. 
assocM x y (M kvs) = M (asociarEn x y kvs)


lookupM :: Eq k => k -> Map k v -> Maybe v
-- PROP: Encuentra un valor dado una clave.
    -- COSTO: O(n).
    -- Siendo kvs la lista de pares clave-valor dada, se realiza una operación lineal sobre la misma. En el peor caso,
    -- (kvs sin la clave o está al final) termina encontrando el valor de la clave dada al final de la lista, resultando 
    -- en que "lookupM" resulta de costo lineal.
lookupM x (M kvs) = buscarValorDeEn x kvs 


deleteM :: Eq k => k -> Map k v -> Map k v
-- PROP: Borra una asociación dada una clave.
    -- COSTO: O(n).
    -- Siendo kvs la lista de pares clave-valor dada, se realiza una operación lineal sobre la misma. En el peor caso,
    -- termina borrando la clave dada al final de la lista, resultando en que "deleteM" sea de costo lineal.
deleteM k (M kvs) = M (borrarAsociacionEn k kvs)


keys :: Map k v -> [k]
-- PROP: Devuelve las claves del map.
    -- COSTO: O(n).
    -- Siendo kvs la lista de pares clave-valor dada, se realiza una operacion lineal sobre la misma. En el peor caso,
    -- resulta de agregar todas las claves de kvs en una lista, lo cual termina siendo de costo lineal, ya que depende
    -- de la cantidad de elementos de kvs.
keys (M kvs) = clavesEn kvs

-- #################################################### AUXILIARES ####################################################

asociarEn :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
    -- COSTO: O(n).
    -- Siendo n la cantidad de pares clave-valor de la lista dada, se realiza una operación de costo constante para cada 
    -- par de la lista dada. En el peor caso, se tiene que asociar el elemento dado al final de la lista, el costo termina
    -- resultando ser lineal.
asociarEn x y []          = [(x, y)]
asociarEn x y ((k,v):kvs) = if x == k 
                               then (x, y):kvs
                               else (k, v):asociarEn x y kvs


buscarValorDeEn :: Eq k => k -> [(k, v)] -> Maybe v
    -- COSTO: O(n).
    -- Siendo n la cantidad de pares clave-valor de la lista dada, se realiza una operación de costo constante para cada 
    -- par de la lista dada. En el peor caso, el valor de la clave dada que se busca está al final de la lista, el costo
    -- termina resultando ser lineal.
buscarValorDeEn x []          = Nothing
buscarValorDeEn x ((k,v):kvs) = if x == k
                                   then Just v
                                   else buscarValorDeEn x kvs 


borrarAsociacionEn :: Eq k => k -> [(k, v)] -> [(k, v)]
    -- COSTO: O(n).
    -- Siendo n la cantidad de pares clave-valor de la lista dada, se realiza una operación de costo constante para cada
    -- par de la lista dada. En el peor caso, la clave dada que se busca está al final de la lista, el costo termina
    -- resultando ser lineal.
borrarAsociacionEn x []          = []
borrarAsociacionEn x ((k,v):kvs) = if x == k
                                     then kvs
                                     else (k,v):borrarAsociacionEn x kvs


clavesEn :: [(k, v)] -> [k]
    -- COSTO: O(n).
    -- Siendo n la cantidad de pares clave-valor de la lista dada, se utiliza la operacion "cons" de cada clave de
    -- los pares de la lista dada. Esto resulta en que "clavesEn" es de costo lineal, ya que por cada elemento de la
    -- lista de pares dada tiene que agregarlos a una nueva lista de claves.
clavesEn []          = []
clavesEn ((k,v):kvs) = k : clavesEn kvs