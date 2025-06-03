-- EJERCICIO A:

data EscuelaDeMagia = EDM (Set Hechizo) (Map Nombre Mago) (PriorityQueue Mago)
    deriving Show

{- INV. REP.:
    * Siendo EDM sh mnm pqm
        * Cada Mago de cada Nombre en MNM tiene que existir también en PQM.
        * Cada Hechizo aprendido por cada Mago de cada Nombre en MNM, tiene que existir en SH.
        * Cada Nombre en MNM debe ser el Nombre correspondiente del Mago asociado como valor.
        * La cantidad de hechizos enseñados en SH no puede ser menor a la cantidad de hechizos enseñados de cualquier mago en MNM y/o PQM.
-}


----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO B:

fundarEscuela :: EscuelaDeMagia
-- PROPÓSITO: Devuelve una escuela vacía.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utilizaron las operaciones "emptyS", "emptyM" y "emptyPQ" (todas de costo constante).
fundarEscuela = EDM emptyS emptyM emptyPQ

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO C:

estaVacia :: EscuelaDeMagia -> Bool
-- PROPÓSITO: Devuelve una escuela vacía.
-- COSTO: O(1).
    -- Siendo M la cantidad de magos y H la cantidad de Hechizos;
estaVacia (EDM _ _ pqm) = isEmptyPQ pqm

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO D:

registar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia
-- PROPÓSITO: Incorpora un mago a la escuela (si ya existe no hace nada).
-- COSTO: O(log M).
    -- Siendo M la cantidad de magos, en M se realizan las operaciones "lookupM", "assocM" e "insertPQ", todas de costo "log M". 
    -- Es por eso que el costo total de la función es "log M".
registar n (EDM sh mnm pqm) = case lookupM n mmn of
                                Just m  -> (EDM sh mnm pqm)
                                Nothing -> let nMago = crearM n;
                                               nMNM = assocM n nMago mnm;
                                               nPQM = insertPQ n pqm
                                            in EDM sh nMNM nPQM

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO E:

magos :: EscuelaDeMagia -> [Nombre]
-- PROPÓSITO: Devuelve los nombres de los magos registrados en la escuela.
-- COSTO: O(M).
    -- Siendo M la cantidad de magos, en M se realiza la operación "domM" de costo "M", es por eso que el costo total de la función
    -- es de costo "M".
magos (EDM _ mnm _) = domM mnm 

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO F:

hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo
-- PROPÓSITO: Devuelve los hechizos que conoce algún mago dado.
-- PRECONDICIÓN: Existe un mago con dicho nombre.
-- COSTO: O(log M).
    -- Siendo M la cantidad de magos, en M se realiza la operación "lookupM" de costo "log M", es por eso que el costo total
    -- de la función es "log M".
hechizosDe n (EDM _ mnm _) = case lookupM n mnm of
                               Nothing -> error "No existe un mago con dicho nombre."
                               Just m  -> hechizos m

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO G:

leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int
-- PROPÓSITO: Dado un mago, indica la cantidad de hechizos que la escuela ha dado y él no sabe.
-- PRECONDICIÓN: Existe un mago con dicho nombre.
-- COSTO: O(log M).
    -- Siendo M la cantidad de magos, en M se realiza la operación "lookupM" de costo "log M", es por eso que el costo total
    -- de la función es "log M". También se utiliza la operación "sizeS" y "hechizos", aunque son de costo constante.
leFaltanAprender n (EDM sh mnm _) = case lookupM n mnm of
                                      Nothing -> error "No existe un mago con dicho nombre."
                                      Just m  -> sizeS sh - sizeS (hechizos m)

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO H:

egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)
-- PROPÓSITO: Devuelve el mago que más hechizos sabe y la escuela sin dicho mago.
-- PRECONDICIÓN: Hay al menos un mago.
-- COSTO: O(log M).
    -- Siendo M la cantidad de magos, en M se utiliza la operación "deleteM" de costo "log M", además de las operaciones "maxPQ",
    -- "deleteMaxPQ", "nombre", todas de costo constante. Es por eso que el costo total de la función es "log M".
egresarUno (EDM sh mnm pqm) = if isEmptyPQ pqm
                                 then error "No hay magos en la Escuela de Magia."
                                 else let elMejorMago = maxPQ pqm;
                                          nMNM = deleteM (nombre elMejorMago) mnm;
                                          nPQM = deleteMaxPQ pqm;
                                          nEDM = EDM sh nMNM nPQM
                                       in (elMejorMago, nEDM)

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO I:

enseñar :: Hechizo -> Nombre -> EscuelaDeMagia -> EscuelaDeMagia
-- PROPÓSITO: Enseña un hechizo a un mago existente, y si el hechizo no existe en la escuela es incorporado a la misma.
-- PRECONDICIÓN: Existe un mago con dicho nombre.
-- COSTO: O(M log M + log H).
    -- Siendo M la cantidad de magos y H la cantidad de Hechizos; en H se realizan las operaciones "addS", "aprender", "belongsS", todas
    -- de costo "log H". En M se realizan las operaciones "assocM" e "InsertPQ" de costo "log M" y la función "deletePQ" de costo "M log M".
    -- Es por eso que el costo total de la función es "M log M + log H".
enseñar h n (EDM sh mnm pqm) = case lookupM n mnm of
                                 Nothing -> error "No existe un mago con dicho nombre."
                                 Just m  -> let nMago = aprender h m;                  -- log H
                                                nSH = addS h sh;                       -- log H
                                                nMNM = assocM n nMago mnm;             -- log M
                                                nPQM = insertPQ nMago (deletePQ n pqm) -- M log M
                                            in if belongsS h sh                        -- log H
                                                  then (EDM sh  nMNM nPQM)
                                                  else (EDM nSH nMNM nPQM)

deletePQ :: Nombre -> PriorityQueue Mago -> PriorityQueue Mago
-- PROPÓSITO: Retorna todos los hechizos aprendidos por los magos.
-- COSTO: O(M log M).
    -- Siendo M la cantidad de magos, por cada M se realizan las operaciones "insertPQ" y "deleteMaxPQ", ambas de costo "log M", además
    -- de utilizar las operaciones "nombre", "isEmptyPQ", "maxPQ", todas de costo constante. Es por eso que el costo total de la 
    -- función es "M log M".
deletePQ n pqm = if isEmptyPQ pqm
                    then pqm
                    else if nombre (maxPQ pqm) == n
                            then deleteMaxPQ pqm
                            else insertPQ (maxPQ pqm) (deletePQ n (deleteMaxPQ pqm))

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO J:

hechizosAprendidos :: EscuelaDeMagia -> Set Hechizo
-- PROPÓSITO: Retorna todos los hechizos aprendidos por los magos.
-- COSTO: O(M * (log M + H log H)).
    -- Siendo M la cantidad de magos y H la cantidad de Hechizos;
hechizosAprendidos edm = hechizosAprendidosM (magos edm) edm 

hechizosAprendidosM :: [Nombre] -> EscuelaDeMagia -> Set Hechizo
-- PROPÓSITO: Retorna todos los hechizos aprendidos por los magos.
-- COSTO: O(M * (log M + H log H)).
    -- Siendo M la cantidad de magos y H la cantidad de Hechizos; por cada M se realiza la operación "hechizosDe" de costo "log M",
    -- y en H se realiza la operación "unionS" de costo "H log H". Es por eso que el costo total de la función es "M * (log M + H log H)".
hechizosAprendidosM []     edm = emptyS
hechizosAprendidosM (n:ns) edm = unionS (hechizosDe n) (hechizosAprendidosM ns edm)

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO K:

hayUnExperto :: EscuelaDeMagia -> Bool
-- PROPÓSITO: Indica si existe un mago que sabe todos los hechizos enseñados por la escuela.
-- COSTO: O(log M).
    -- Siendo M la cantidad de magos, en M se utilizan las operaciones "egresarUno" y "leFaltanAprender" de costo "log M", es por eso
    -- que el costo total de la función es "log M".
hayUnExperto edm = let magoQueMasSabe = fst (egresarUno edm)
                    in leFaltanAprender (nombre magoQueMasSabe) edm == 0

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO L:

egresarExpertos :: EscuelaDeMagia -> ([Mago], EscuelaDeMagia)
-- PROPÓSITO: Devuelve un par con la lista de magos que saben todos los hechizos dados por la escuela y la escuela sin dichos magos.
-- COSTO: O(M log M).
    -- Siendo M la cantidad de magos, por cada M se utilizan las operaciones "egresarUno" y "leFaltanAprender" de costo "log M", 
    -- es por eso que el costo total de la función es "M log M".
egresarExpertos edm = if estaVacia edm
                         then ([], edm)
                         else let (m, edmSinM) = egresarUno edm;
                                  (expertosRestantes, edmFinal) = egresarExpertos edmSinM
                               in if leFaltanAprender (nombre m) edm == 0
                                     then (m:expertosRestantes, edmFinal)
                                     else (expertosRestantes, edmFinal)

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO M:

-- Esta es una posible implementación del tipo Mago:

data Mago = Nombre (Set Hechizo)
    deriving Show