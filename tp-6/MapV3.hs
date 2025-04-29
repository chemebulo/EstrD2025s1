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

-- #################################################### IMPLEMENTACIÓN #################################################### 

emptyM :: Map k v
-- PROP: Devuelve un map vacío.
    -- COSTO: O(1).
    -- Siendo en el peor caso de costo constante por el hecho de solamente utilizar el constructor con dos listas vacías.
emptyM = M [] []


assocM :: Eq k => k -> v -> Map k v -> Map k v
-- PROP: Agrega una asociación clave-valor al map.
    -- COSTO: O(n).
    -- Siendo ks1 la lista de claves dada, y vs1 la lista de valores dado, se realiza una operación de costo lineal para
    -- ks1 y vs1, utilizando además k y v (clave y valor nuevo dado). Siempre recorre hasta el final por diseño, incluso 
    -- si no encuentra la clave, y después la agrega igual, lo cual termina resultando en que "assocM" sea de costo lineal.
assocM k v (M ks1 vs1) = let (ks, vs) = agregarEn k v ks1 vs1
                         in M ks vs


lookupM :: Eq k => k -> Map k v -> Maybe v
-- PROP: Encuentra un valor dado una clave.
    -- COSTO: O(n).
    -- Siendo ks la lista de claves dada, y vs la lista de valores dado, se realiza una operación de costo lineal para
    -- ks y vs, utilizando además k (clave dada), lo cual termina resultando en que "lookupM" sea de costo lineal.
lookupM k (M ks vs) = valueEn k ks vs


deleteM :: Eq k => k -> Map k v -> Map k v
-- PROP: Borra una asociación dada una clave.
    -- COSTO: O(n).
    -- Siendo ks la lista de claves dada, y vs la lista de valores dado, se realiza una operacion de costo lineal para
    -- ks y vs, utilizando además k (clave dada), lo cual termina resultando en que "deleteM" sea de costo lineal. 
deleteM k (M ks ys) = let (ks, vs) = eliminarM k ks vs
                      in M ks vs


keys :: Map k v -> [k]
-- PROP: Devuelve las claves del map.
    -- COSTO: O(1).
    -- Siendo ks la lista de claves dada, y vs la lista de valores dado, solamente se devuelve ks, lo cual es de costo
    -- constante.
keys (M ks vs) = ks

-- #################################################### AUXILIARES ####################################################

agregarEn :: Eq k => k -> v -> [k] -> [v] -> ([k],[v])
-- PRECOND: Las listas dadas deben tener la misma longitud.
    -- COSTO: O(n).
    -- Siendo ks la lista de claves dada, y vs la lista de valores dado, se realiza una operación de comparación de costo
    -- constante para cada clave de ks. En el peor caso, la clave a la que se le quiere agregar un nuevo valor está al final 
    -- de la lista de ks o se termina llegando al final de la lista y no esta la clave dada. Esto resulta en que "agregarEn"
    -- es de costo lineal.
agregarEn nk nv []     _      = ([nk], [nv])
agregarEn nk nv (k:ks) (v:vs) = if nk == k
                                   then (k:ks, nv:vs)
                                   else let (ks', vs') = agregarEn nk nv ks vs
                                        in  (k:ks', v:vs')


valueEn :: Eq k => k -> [k] -> [v] -> Maybe v
-- PRECOND: Las listas dadas deben tener la misma longitud.
    -- COSTO: O(n).
    -- Siendo ks la lista de claves dada, y vs la lista de valores dado, se realiza una operación de comparación de costo
    -- constante para cada clave de ks. El peor caso es que el valor de la clave a buscar está al final de la lista o que
    -- en realidad llegue al final de la lista y no esté. Es por eso que "valueEn" termina resultando ser de costo lineal.
valueEn nk []     _      = Nothing
valueEn nk (k:ks) (v:vs) = if nk == k
                              then Just v
                              else valueEn nk ks vs  


eliminarM :: Eq k => k -> [k] -> [v] -> ([k],[v])
-- PRECOND: Las listas dadas deben tener la misma longitud.
    -- COSTO: O(n).
    -- Siendo ks la lista de claves dada, y vs la lista de valores dado, se realiza una operación de comparación de costo
    -- constante para cada clave de ks. El peor caso es que la clave a buscar está al final de la lista o que en realidad
    -- llegue al final de la lista y no esté. Es por eso que "eliminarM" resulta ser de costo lineal.
eliminarM nk [] _          = ([], [])
eliminarM nk (k:ks) (v:vs) = if nk == k
                                then (ks, vs)
                                else let (ks', vs') = eliminarM nk ks vs
                                     in  (k:ks', v:vs')