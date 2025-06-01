-- EJERCICIO: INVARIANTES DE REPRESENTACIÓN.

data RAList a = MKR Int (Map Int a) (Heap a)
    deriving Show

{- INV. REP.:
    * Siendo MKR n mia mha:
        * Si MIA tiene al menos una clave, siempre comienza desde el número 0 en adelante. 
        * El valor "n" tiene que ser siempre uno menos que la última clave en MIA, en caso de que MIA esté vacío va a ser 0.
        * Todos los valores en MHA tienen que estar como valor de su clave correspondiente en MIA.
        * Cada valor de su clave correspondiente en MIA tiene que estar como valor en MHA.
        * En MIA deben existir al menos "n-1" claves, salvo que esté vacío y en ese caso debe ser 0.
-}

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO A:

emptyRAL :: RaList a
-- PROPÓSITO: Devuelve una lista vacía.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza las operaciones "emptyM" y "emptyH", ambas de costo constante.
emptyRAL = MKR 0 emptyM emptyH

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO B:

isEmptyRAL :: RAList a -> Bool
-- PROPÓSITO: Indica si la lista está vacía.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se realiza una comparación con el valor de "n".
isEmptyRAL (MKR n _ _) = n == 0

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO C:

lenghtRAL :: RAList a -> Int
-- PROPÓSITO: Devuelve la cantidad de elementos.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se devuelve el valor de "n".
lenghtRAL (MKR n _ _) = n

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO D:

get :: Int -> RAList a -> a
-- PROPÓSITO: Devuelve el elemento en el índice dado.
-- PRECONDICIÓN: El índice debe existir.
-- COSTO: O(log N).
    -- Siendo N la cantidad de elementos, en N se realiza la operación "lookupM" de costo "log N", es por eso que el costo total
    -- de la función es "log N".
get m (MKR _ mia _) = case lookupM m mia of
                        Just x  -> x
                        Nothing -> error "El índice dado no existe."

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO E:

minRAL :: Ord a => RAList a -> a 
-- PROPÓSITO: Devuelve el mínimo elemento de la lista.
-- PRECONDICIÓN: La lista no está vacía.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza la operación "findMin" de costo constante.
minRAL (MKR _ _ mha) = findMin mha

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO F:

add :: Ord a => a -> RAList a -> RAList a
-- PROPÓSITO: Agrega un elemento al final de la lista.
-- COSTO: O(log N).
    -- Siendo N la cantidad de elementos, en N se realizan las operaciones "assocM" e "insertH" ambas de costo "log N", es por eso
    -- que el costo total de la función es "log N".
add x (MKR n mia mha) = let nN = n+1;              -- 1
                            nMIA = assocM n x mia; -- log N
                            nMHA = insertH x mha   -- log N
                         in MKR nN nMIA nMHA

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO G:

elems :: Ord a => RAList a -> [a]
-- PROPÓSITO: Transforma una RAList en una lista, respetando el orden de los elementos.
-- COSTO: O(N log N).
    -- Siendo N la cantidad de elementos, en N se utiliza la función "elemsMIA" de costo "N log N", es por eso que el costo total
    -- de la función es "N log N".
elems (MKR n mia _) = elemsMIA 0 n mia

elemsMIA :: Ord a => Int -> Int -> Map Int a -> [a]
-- PROPÓSITO: Transforma un Map en una lista, respetando el orden de los elementos.
-- COSTO: O(N log N).
    -- Siendo N la cantidad de elementos, por cada N se realiza la operación "lookupM" de costo "log N", es por eso que el costo
    -- total de la función es "N log N".
elemsMIA m n mia = if m > (n-1) 
                      then []
                      else case lookupM m mia of
                             Just x  -> x : elemsMIA (m+1) n mia
                             Nothing -> error "No debería dar este error."

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO H:

remove :: Ord a => RAList a -> RAList a
-- PROPÓSITO: Elimina el último elemento de la lista.
-- PRECONDICIÓN: La lista no está vacía.
-- COSTO: O(N log N).
    -- Siendo N la cantidad de elementos, en N se realizan las operaciones "lookupM" y "deleteM" ambas de costo "log N", además se
    -- utiliza la función "removeH" de costo "N log N". Es por eso que el costo total de la función es "N log N + log N", aunque
    -- se simplifica como "N log N".
remove (MRK n mia mha) = case lookupM (n-1) mia of                  -- log N 
                           Nothing -> error "La lista está vacía."
                           Just x  -> let nN = n-1;                 -- 1
                                          nMIA = deleteM (n-1) mia; -- log N
                                          nMHA = removeH x mha      -- N log N
                                       in MRK nN nMIA nMHA

removeH :: Ord a => a => Heap a => Heap a
-- PROPÓSITO: Elimina el elemento dado de la Heap dada.
-- COSTO: O(N log N).
    -- Siendo N la cantidad de elementos, por cada N se realizan las operaciones "insertH" y "removeH", ambas de costo "log N",
    -- es por eso que el costo total de la función es "N log N". También se utilizan las operaciones "findMin" y "deleteMin"
    -- aunque ambas son de costo constante.
removeH x mha = if findMin mha == x
                   then deleteMin mha
                   else insertH (findMin mha) (removeH x (deleteMin mha))

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO I:

set :: Ord a => Int -> a -> RAList a -> RAList a
-- PROPÓSITO: Reemplaza el elemento en la posición dada.
-- PRECONDICIÓN: El índice debe existir.
-- COSTO: O(N log N).
    -- Siendo N la cantidad de elementos, en N se utilizan las operaciones "lookupM" y "assocM" ambas de costo "log N", además
    -- se utiliza la función "removeH" de costo "N log N" y la operación "insertH" de costo "log N". Es por eso que el costo
    -- total de la función es "N log N".
set m x (MKR n mia mha) = case lookupM m mia of
                            Nothing -> error "El índice dado no existe."
                            Just y  -> let nMIA = assocM m x mia;            -- log N
                                           nMHA = insertH x (removeH y mha); -- N log N
                                        in MKR n nMIA nMHA

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO J:

addAt :: Ord a => Int -> a -> RAList a -> RAList a
-- PROPÓSITO: Agrega un elemento en la posición dada.
-- PRECONDICIÓN: El índice debe estar entre 0 y la longitud de la lista.
-- OBSERVACIÓN: Cada elemento en una posición posterior a la dada pasa a estar en su posición siguiente.
-- COSTO: O(N log N).
    -- Siendo N la cantidad de elementos, en N se utilizan las operaciones "assocM" e "insertH" ambas de costo "log N", además
    -- se utiliza la función "prepararMIA" de costo "N log N". Es por eso que el costo total de la función es "N log N".
addAt m x (MRK n mia mha) = let nN = n+1;                                -- 1
                                nMIA = assocM m x (prepararMIA n m mia); -- N log N
                                nMHA = insertH x mha                     -- log N
                             in MRK nN nMIA nMHA

prepararMIA :: Ord a => Int -> Int -> Map Int a -> Map Int a
-- PROPÓSITO: Prepara el Map dado para que le agreguen un elemento en la posición dada, es decir, mueve todos sus elementos desde 
--            la posición dada en uno hacia adelante.
-- PRECONDICIÓN: El índice debe estar entre 0 y la longitud de la lista.
-- OBSERVACIÓN: El parámetro "m" representa la posición dada, y el parámetro "n" representa la máxima posición posible.
-- COSTO: O(N log N).
    -- Siendo N la cantidad de elementos, por cada N se realizan las operaciones "lookupM" y "assocM" ambas de costo "log N", 
    -- es por eso que el costo total de la función es "N log N".
prepararMIA n m mia = if n < m
                         then mia
                         else case lookupM n mia of
                                Just x  -> assocM (n+1) x (prepararMIA (n-1) m mia)
                                Nothing -> error "El índice es inválido."