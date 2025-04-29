-- EJERCICIO 1:

-- El costo de heapsort :: Ord a => [a] -> [a] es 'n log n', ya que se toma el peor caso de eficiencia de las funciones utilizadas.

    {- 
        heapSort :: Ord a => [a] -> [a]
        -- PROP: Dada una lista, la ordena de menor a mayor utilizando una Priority Queue como estructura auxiliar.
            -- COSTO: O(n log n) que es lo mismo que (n*2 log n).
            -- Siendo n la cantidad de elementos de la lista dada, se utiliza en toda la lista "listaAPriorityQueue" de costo 'n log n',
            -- y para la priority queue resultante "priorityQueueALista" de costo 'n log n'.     
        heapSort xs = priorityQueueALista (listaAPriorityQueue xs)
        
        
        priorityQueueALista :: Ord a => PriorityQueue a -> [a]
            -- COSTO: O(n log n).
            -- Siendo n la cantidad de elementos de la priority queue, para cada elemento se utiliza "findMinPQ" de costo costante y se elimina 
            -- el mínimo de la lista dada utilizando "deleteMinPQ" de costo 'log n'; y también se utiliza "isEmptyPQ" de costo constante. 
        priorityQueueALista pq = if not (isEmptyPQ pq)
                                    then findMinPQ pq : priorityQueueALista (deleteMinPQ pq)
                                    else []
        
        
        listaAPriorityQueue :: Ord a => [a] -> PriorityQueue a
            -- COSTO: O(n log n). 
            -- Siendo n la cantidad de elementos de la lista dada, se utiliza "insertPQ" de costo 'log n' en cada uno de ellos.  
        listaAPriorityQueue []     = emptyPQ
        listaAPriorityQueue (x:xs) = insertPQ x (listaAPriorityQueue xs)
    -}


-- EJERCICIO 2:

---------------------------- AUXILIARES ----------------------------
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show
--------------------------------------------------------------------
inorderT :: Tree a -> [a]
inorderT EmptyT          = []
inorderT (NodeT x ti td) = inorderT ti ++ [x] ++ inorderT td
--------------------------------------------------------------------


belongsBST :: Ord a => a -> Tree a -> Bool
-- PROP: Dado un BST dice si el elemento pertenece o no al árbol.
-- PRECOND: Cumple los invariantes de BST y no tiene elementos repetidos.
    -- COSTO: O(log n)
    -- Siendo...
belongsBST = undefined


insertBST :: Ord a => a -> Tree a -> Tree a
-- PROP: Dado un BST inserta un elemento en el árbol.
-- PRECOND: Cumple los invariantes de BST y no tiene elementos repetidos.
    -- COSTO: O(log n)
    -- Siendo...
insertBST = undefined


deleteBST :: Ord a => a -> Tree a -> Tree a
-- PROP: Dado un BST borra un elemento en el árbol.
-- PRECOND: Cumple los invariantes de BST y no tiene elementos repetidos.
    -- COSTO: O(log n)
    -- Siendo...
deleteBST = undefined


splitMinBST :: Ord a => Tree a -> (a, Tree a)
-- PROP: Dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
-- PRECOND: Cumple los invariantes de BST y no tiene elementos repetidos.
    -- COSTO: O(log n)
    -- Siendo...
splitMinBST = undefined


splitMaxBST :: Ord a => Tree a -> (a, Tree a)
-- PROP: Dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
-- PRECOND: Cumple los invariantes de BST y no tiene elementos repetidos.
    -- COSTO: O(log n)
    -- Siendo...
splitMaxBST = undefined


esBST :: Tree a -> Bool
-- PROP: Indica si el árbol cumple con los invariantes de BST.
-- PRECOND: Cumple los invariantes de BST y no tiene elementos repetidos.
    -- COSTO: O(n^2)
    -- Siendo...
esBST = undefined


elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
-- PROP: Dado un BST y un elemento, devuelve el máximo elemento que sea menor al elemento dado.
-- PRECOND: Cumple los invariantes de BST y no tiene elementos repetidos.
    -- COSTO: O(log n)
    -- Siendo...
elMaximoMenorA = undefined


elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
-- PROP: Dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al elemento dado.
-- PRECOND: Cumple los invariantes de BST y no tiene elementos repetidos.
    -- COSTO: O(log n)
    -- Siendo...
elMinimoMayorA = undefined


balanceado :: Tree a -> Bool
-- PROP: Indica si el árbol está balanceado. Un árbol está balanceado cuando para cada nodo la diferencia de
--       alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
-- PRECOND: Cumple los invariantes de BST y no tiene elementos repetidos.
    -- COSTO: O(n^2)
    -- Siendo...
balanceado = undefined


-- EJERCICIO 3:




-- EJERCICIO 4:

-- import Map
-- import Set
-- 
-- type SectorId = Int
-- 
-- type CUIL = Int
-- 
-- data Empresa = ConsE (Map SectorId (Set Empleado))
-- --                   (Map   CUIL      Empleado)
--     deriving Show


-- EJERCICIO 5:

