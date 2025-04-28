-- EJERCICIO 1:

-- El costo de heapsort :: Ord a => [a] -> [a] es 'n log n', ya que se toma el peor caso de eficiencia de las funciones utilizadas.
    
    -- heapSort :: Ord a => [a] -> [a]
    -- -- PROP: Dada una lista, la ordena de menor a mayor utilizando una Priority Queue como estructura auxiliar.
    --     -- COSTO: O(n log n) que es lo mismo que (n*2 log n).
    --     -- Siendo n la cantidad de elementos de la lista dada, se utiliza en toda la lista "listaAPriorityQueue" de costo 'n log n',
    --     -- y para la priority queue resultante "priorityQueueALista" de costo 'n log n'.     
    -- heapSort xs = priorityQueueALista (listaAPriorityQueue xs)
    -- 
    -- 
    -- priorityQueueALista :: Ord a => PriorityQueue a -> [a]
    --     -- COSTO: O(n log n).
    --     -- Siendo n la cantidad de elementos de la priority queue, para cada elemento se utiliza "findMinPQ" de costo costante y se elimina 
    --     -- el mínimo de la lista dada utilizando "deleteMinPQ" de costo 'log n'; y también se utiliza "isEmptyPQ" de costo constante. 
    -- priorityQueueALista pq = if not (isEmptyPQ pq)
    --                             then findMinPQ pq : priorityQueueALista (deleteMinPQ pq)
    --                             else []
    -- 
    -- 
    -- listaAPriorityQueue :: Ord a => [a] -> PriorityQueue a
    --     -- COSTO: O(n log n). 
    --     -- Siendo n la cantidad de elementos de la lista dada, se utiliza "insertPQ" de costo 'log n' en cada uno de ellos.  
    -- listaAPriorityQueue []     = emptyPQ
    -- listaAPriorityQueue (x:xs) = insertPQ x (listaAPriorityQueue xs)
    

-- EJERCICIO 2:




-- EJERCICIO 3:




-- EJERCICIO 4:




-- EJERCICIO 5:

