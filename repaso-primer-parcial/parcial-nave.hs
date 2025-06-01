-- EJERCICIO A:

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)
    deriving Show

{- INV. REP.:
    * Siendo N mss mnt mht:
        * Cada Tripulante de cada Nombre en MNT, debe estar también en MHT.
        * Cada Tripulante que esté en MHT debe estar también como Tripulante del Nombre del mismo en MNT.
        * Cada Nombre en MNT tiene debe coincidir con el Nombre del Tripulante que tiene como valor. 
        * Cada Nombre en cada Sector de cada SectorId, debe estar como Nombre en MNT y como Nombre del Tripulante en MHT. 
        * Cada SectorId asignado en cada Tripulante de cada Nombre en MNT, debe estar asignado el Nombre en cada SectorId en MSS. 
        * Cada SectorId debe coincidir con el Sector que tiene asignado en MSS.
        * Cada Sector debe coincidir con el SectorId que tiene como clave en MSS.
-}

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO B:

construir :: [SectorId] -> Nave
-- PROPÓSITO: Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
-- COSTO: O(S log S).
    -- Siendo S la cantidad de SectorId, en S se utiliza la función "construirS" de costo "S log S", es por eso que el
    -- costo total de la función es "S log S". También se utilizan las operaciones "emptyM" y "emptyH", pero son de costo constante.
construir ss = let nMSS = construirS ss
                in N nMSS emptyM emptyH 


construirS :: [SectorId] -> Map SectorId Sector
-- PROPÓSITO: Construye un Map con sectores vacíos, en base a una lista de identificadores de sectores.
-- COSTO: O(S log S).
    -- Siendo S la cantidad de SectorId, por cada S se realiza la operación "assocM" de costo "log S", es por eso
    -- que el costo total de la función es "S log S".
construirS []     = emptyM
construirS (s:ss) = assocM s (crearS s) (construirS ss)

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO C:

ingresarT :: Nombre -> Rango -> Nave -> Nave
-- PROPÓSITO: Incorpora un tripulante a la nave, sin asignarle un sector.
-- COSTO: O(log T + log N).
    -- Siendo T la cantidad de Tripulantes y N la cantidad de Nombres; en N se realiza la operación "assocM" de costo "log N",
    -- y después en T se utiliza la operación "insertH" de costo "log T". Siendo además que se utiliza la operación "crearT" de 
    -- costo constante. Esto termina resultando con que el costo total de la función sea "log T + log N".
ingresarT n r (N mss mnt mht) = let nTrip = crearT n r;
                                    nMNT = assocM n nTrip mnt;
                                    nMHT = insertH nTrip mht 
                                 in N mss nMNT nMHT

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO D:

sectoresAsignados :: Nombre -> Nave -> Set SectorId
-- PROPÓSITO: Devuelve los sectores asignados a un tripulante.
-- PRECONDICIÓN: Existe un tripulante con dicho nombre. 
-- COSTO: O(log N).
    -- Siendo N la cantidad de Nombres, en N se realiza la operación "lookupM" de costo "log N", es por eso que el costo
    -- total de la función es "log N". También se utilizó la operación "sectoresT" pero es de costo constante.
sectoresAsignados n (N mss mnt mht) = case lookupM n mnt of
                                        Just t  -> sectoresT t
                                        Nothing -> error "No existe ningún tripulante con ese nombre." 

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO E:

datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
-- PROPÓSITO: Dado un Sector, devuelve los Tripulantes y los Componententes asignados a ese Sector.
-- PRECONDICIÓN: Existe un Sector con dicho Id.
-- COSTO: O(log S).
    -- Siendo S la cantidad de SectorId, en S se realiza la operación "lookupM" de costo "log S", es por eso que el costo
    -- total de la función es "log S". También se utilizó la operación "tripulantesS" y "componentesS", pero ambas son de costo constante.
datosDeSector sid (N mss mnt mht) = case lookupM sid mss of
                                      Just s  -> (tripulantesS s, componentesS s)
                                      Nothing -> error "No existe un Sector con dicho Id."

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO F:

tripulantesN :: Nave -> [Tripulante]
-- PROPÓSITO: Devuelve una lista de Tripulantes ordenada por rango, de mayor a menor.
-- COSTO: O(T log T).
    -- Siendo T la cantidad de Tripulantes, ...
tripulantesN (N mss mnt mht) = tripulantesMHT mht 

tripulantesMHT :: MaxHeap Tripulante -> [Tripulante]
-- PROPÓSITO: Devuelve una lista con los Tripulantes de la MaxHeap Tripulante dada.
-- COSTO: O(T log T).
    -- Siendo T la cantidad de Tripulantes, por cada T se realiza la operación "deleteMaxH" de costo "log T", además de utilizarse
    -- la operación "isEmptyH" de costo constante. Es por eso, que el costo total de la función es "T log T".
tripulantesMHT mht = if (isEmptyH mht)
                        then []
                        else maxH mht : tripulantesMHT (deleteMaxH mht)

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO G:

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
-- PROPÓSITO: Asigna una lista de Componentes a un Sector de la Nave.
-- COSTO: O(C + log S).
    -- Siendo S la cantidad de SectorId y C la cantidad de Componentes; en S se realiza la operación "assocM" de costo "log S",
    -- y en C se utiliza la función "agregarComponentesSector" de costo "C". Es por eso que el costo total de la función es "C + log S".
agregarASector cs sid (N mss mnt mht) = case lookupM sid mss of
                                          Just s  -> let nSector = agregarComponentesSector cs s;
                                                         nMSS = assocM sid nSector mss
                                                      in N nMSS mnt mht
                                          Nothing -> error "No existe un Sector con dicho Id."

agregarComponentesSector :: [Componente] -> Sector -> Sector
-- PROPÓSITO: Agrega los Componentes al Sector dado.
-- COSTO: O(C).
    -- Siendo C la cantidad de Componentes, por cada C se realiza la operación "agregarC" de costo constante, es por eso
    -- que el costo total de la funcion es "C"; ya que por cada elemento de la lista se realiza una operación de costo constante. 
agregarComponentesSector []     s = s
agregarComponentesSector (c:cs) s = agregarC c (agregarComponentesSector cs s)  

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO H:

asignarASector :: Nombre -> SectorId -> Nave -> Nave
-- PROPÓSITO: Asigna un Sector a un Tripulante.
-- PRECONDICIÓN: El Tripulante y el Sector existen.
-- NOTA: No importa si el Tripulante ya tiene asignado dicho Sector.
-- COSTO: O(log S + log N + T log T).
    -- Siendo S la cantidad de SectorId, N la cantidade de Nombres y T la cantidad de Tripulantes; en S se realizan las operaciones 
    -- "lookupM", "assocM", "asignarS", todas de costo "log S". En N se realizan las operaciones "lookupM", "agregarT" y "assocM",
    -- todas de costo "log N". También, en T se utiliza la función "reemplazarT" de costo "T log T". Es por eso que el costo total
    -- de la función es "log S + log N + T log T".
asignarASector n sid (N mss mnt mht) = case lookupM sid mss of                                      -- log S
                                         Nothing -> error "No existe un Sector con dicho Id."
                                         Just s  -> case lookupM n mnt of                           -- log N
                                                      Nothing -> error "No existe un Tripulante con dicho Nombre."
                                                      Just t  -> let nSector = agregarT n s;        -- log N
                                                                     nTrip = asignarS sid t;        -- log S
                                                                     nMSS = assocM sid nSector mss; -- log S
                                                                     nMNT = assocM n nTrip mnt;     -- log N
                                                                     nMHT = reemplazarT nTrip mht   -- T log T
                                                                  in N nMSS nMNT nMHT

reemplazarT :: Tripulante -> MaxHeap Tripulante -> MaxHeap Tripulante
-- PROPÓSITO: Reemplaza el Tripulante existente en la MaxHeap Tripulante dada por el Tripulante dado. En el caso que no exista,
--            lo agrega a la MaxHeap.
-- COSTO: O(T log T).
    -- Siendo T la cantidad de Tripulantes, por cada T se realiza la operación "insertH" y "deleteMaxH", ambas de costo "log T",
    -- es por eso que el costo total de la función es "T log T".
reemplazarT t mht = if (nombre (maxH mht)) == (nombre t) 
                       then insertH t (deleteMaxH mht) 
                       else insertH (maxH mht) (reemplazarT t (deleteMaxH mht))

----------------------------------------------------------------------------------------------------------------------------------------
-- COMO USUARIO:
----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO I:

sectores :: Nave -> Set SectorId
-- PROPÓSITO: Devuelve todos los sectores no vacíos (con tripulantes asignados).
-- COSTO: O(T * S log S + log T).
    -- Siendo T la cantidad de Tripulantes y S la cantidad de SectorId; se utiliza la función "sectoresTS" de costo "T * S log S",
    -- y también "tripulantesN" de costo "log T". Es por eso que el costo total de la función es "T * S log S + log T".
sectores n = sectoresTS (tripulantesN n)

sectoresTS :: [Tripulante] -> Set SectorId
-- PROPÓSITO: Devuelve todos los sectores no vacíos (con tripulantes asignados).
-- COSTO: O(T * S log S).
    -- Siendo T la cantidad de Tripulantes y S la cantidad de SectorId; por cada T se realiza la operación "sectoresT" de costo
    -- constante, y la operación "unionS" de costo "S log S". Es por eso que el costo de la función es "T * S log S".
sectoresTS []     = emptyS
sectoresTS (t:ts) = unionS (sectoresT t) (sectoresTS ts)

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO J:

sinSectoresAsignados :: Nave -> [Tripulante]
-- PROPÓSITO: Devuelve los tripulantes que no poseen sectores asignados.
-- COSTO: O(T + log T).
    -- Siendo T la cantidad de Tripulantes, en T se utiliza la función "sinSectoresAsignadosTS" de costo "T", y también la operación
    -- "tripulantesN" de costo "log T". Es por eso que el costo total de la función sea "T + log T". 
sinSectoresAsignados n = sinSectoresAsignadosTS (tripulantesN n)

sinSectoresAsignadosTS :: [Tripulante] -> [Tripulante]
-- PROPÓSITO: Devuelve los tripulantes que no poseen sectores asignados.
-- COSTO: O(T).
    -- Siendo T la cantidad de Tripulantes, por cada T se realiza la operación "sectoresT" y "sizeS" de costo constante, es 
    -- por eso que el costo total de la función es "T". 
sinSectoresAsignadosTS []     = []
sinSectoresAsignadosTS (t:ts) = if sizeS (sectoresT t) == 0
                                   then t : sinSectoresAsignadosTS ts
                                   else sinSectoresAsignadosTS ts

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO K:

barriles :: Nave -> [Barril]
-- PROPÓSITO: Devuelve todos los barriles de los sectores asignados de la nave.
-- COSTO: O(S log S + T * S log S + C).
    -- Siendo S la cantidad de SectorId y C la cantidad de Componentes; se utiliza la función "sectores" de costo "T * S log S + log T"
    -- además la operación "setToList" de costo "S", la función "componentesSID" de costo "S * log S + S", y la función "barrilesC"
    -- de costo "C". Es por eso que el costo total de la función es "S * log S + S + T * S log S + log T + C", pero se puede 
    -- simplificar en "S log S + T * S log S + C".
barriles n = barrilesC (componentesSID n (setToList (sectores n)))

componentesSID :: Nave -> [SectorId] -> [Componente]
-- PROPÓSITO: Devuelve todos los componentes de los sectores asignados de la nave.
-- COSTO: O(S log S).
    -- Siendo S la cantidad de SectorId, por cada S se realiza la operación "datosDeSector" de costo "log S", y la operación "++"
    -- de costo "S". Es por eso que el costo total de la función es "S * log S + S", pero se puede simplificar en "S log S".
componentesSID n []     = []
componentesSID n (s:ss) = snd (datosDeSector s n) ++ componentesSID n ss

barrilesC :: [Componente] -> [Barril]
-- PROPÓSITO: Devuelve todos los barriles de los componentes dados.
-- COSTO: O(C).
    -- Siendo C la cantidad de Componentes, por cada C se realiza en el peor caso, la operación "++" de costo lineal. Es por eso
    -- que el costo de la función es "C".
barrilesC []     = []
barrilesC (c:cs) = case c of
                     Almacen b -> b ++ barrilesC cs
                     _         -> barrilesC cs

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO L:

-- Una posible implementación podría ser: 

data Sector = S SectorId [Componente] Set Nombre
    deriving Show