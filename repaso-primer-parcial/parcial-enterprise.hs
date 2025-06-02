-- EJERCICIO 1.A:

tripulantes :: Nave -> Set Tripulante
-- PROPÓSITO: Denota los tripulantes de la nave.
-- COSTO: O(S * S log S).
    -- Siendo S la cantidad de Sectores, en S se realiza la operación "sectores" de costo "S", y se utiliza la función "tripulantesS"
    -- de costo "S * S log S", es por eso que el costo total de la función es "S * S log S".
tripulantes n = tripulantesS (sectores n) n

tripulantesS :: [Sector] -> Nave -> Set Tripulante
-- PROPÓSITO: Describe los tripulantes que estan en cada sector de la nave.
-- COSTO: O(S * S log S).
    -- Siendo S la cantidad de Sectores, por cada Sector se realiza la operación "unionS" de costo "S log S" y además "tripulantesDe"
    -- de costo "log S". Es por eso que el costo total de la función es "S * S log S".
tripulantesS []     n = emptyS
tripulantesS (s:ss) n = unionS (tripulantesDe s n) (tripulantesS ss n)

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 1.B:

bajaDeTripulante :: Tripulante -> Nave -> Nave
-- PROPÓSITO: Elimina al tripulante de la nave.
-- COSTO: O(P * S log S + log P).
    -- Siendo S la cantidad de Sectores, y P la cantidad de Tripulantes; en S se utiliza la operación "sectores" de costo "S", y además
    -- se utiliza la función "bajaDeTripulanteS" de costo "P * S log S + log P", es por eso que el costo total de la función es ese.
bajaDeTripulante t n = bajaDeTripulanteS (naveVacia (sectores n)) (sectores n) n t

bajaDeTripulanteS :: Nave -> [Sector] -> Nave -> Tripulante -> Nave
-- PROPÓSITO: Elimina el tripulante dado, volviendo a armar la nave sin él.
-- COSTO: O(P * S log S + log P).
    -- Siendo S la cantidad de Sectores, y P la cantidad de Tripulantes; en S se realiza la operación "tripulantesDe" de costo "log S",
    -- sobre P se realiza la operación "setToList" de costo "P". Además, se utiliza la función "agregarTripulantesSin" de costo
    -- "P * S log S + log P". Es por eso que el costo total de la función es "P * S log S + log P".
bajaDeTripulanteS nn []     nv t = nn
bajaDeTripulanteS nn (s:ss) nv t = agregarTripulantesSin (setToList (tripulantesDe s nv)) s (bajaDeTripulanteS nn ss nv t) t

agregarTripulantesSin :: [Tripulante] -> Sector -> Nave -> Tripulante -> Nave
-- PROPÓSITO: Describe una nave sin el tripulante dado.
-- COSTO: O(P * S log S + log P).
    -- Siendo S la cantidad de Sectores, y P la cantidad de Tripulantes; por cada P se realiza la operación "agregarTripulante" de
    -- costo "S log S + log P", es por eso que el costo total de la función es "P * S log S + log P".
agregarTripulantesSin []     s nn tq = nn
agregarTripulantesSin (t:ts) s nn tq = if t == tq
                                          then agregarTripulantesSin ts s nn tq
                                          else agregarTripulante t s (agregarTripulantesSin ts s nn tq)

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 2.A:

data Nave = MKN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int)
    deriving Show

{- INV. REP.:
    * Siendo MKN msst mht si:
        * Todos los Tripulantes de cada Sector en MSST deben existir también en MHT.
        * Todos los Tripulantes en MHT deben existir también en cada Sector correspondiente de MSST.
        * Cada Tripulante puede estar en un Sector como máximo.
        * El Sector del par "SI" debe existir en MSST y tener la misma cantidad de Tripulantes del par.
        * Para que la nave exista, es necesario que en MSST haya al menos un Sector.
-}

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 2.B:

naveVacia :: [Sector] -> Nave
-- PROPÓSITO: Crea una nave con todos esos sectores sin tripulantes.
-- COSTO: O(S log S).
    -- Siendo S la cantidad de Sectores, en S se utiliza la función "agregarSectoresN" de costo "S log S", y también se utilizan las
    -- operaciones "emptyH" y "head", ambas de costo constante. Es por eso que el costo total de la función es "S log S".
naveVacia ss = let nMSST = agregarSectoresN ss
                in MKN nMSST emptyH (head ss, 0)

agregarSectoresN :: [Sector] -> Map Sector (Set Tripulante)
-- PROPÓSITO: Crea una Map con los sectores dados sin tripulantes. 
-- COSTO: O(S log S).
    -- Siendo S la cantidad de Sectores, por cada S se realiza la operación "assocM" de costo "log S", es por eso que el costo
    -- total de la función es "S log S".
agregarSectoresN []     = emptyM
agregarSectoresN (s:ss) = assocM s emptyS (agregarSectoresN ss)

----------------------------------------------------------------------------------------------------------------------------------------

tripulantesDe :: Sector -> Nave -> Set Tripulante
-- PROPÓSITO: Obtiene los tripulantes de un sector.
-- COSTO: O(log S).
    -- Siendo S la cantidad de Sectores, en S se realiza la función "lookupM" de costo "log S". Es por eso que el costo total
    -- de la función es "log S".
tripulantesDe s (MKN msst _ _) = case lookupM s msst of
                                   Just st -> st 
                                   Nothing -> error "No existe dicho Sector en la Nave."

----------------------------------------------------------------------------------------------------------------------------------------

sectores :: Nave -> [Sector]
-- PROPÓSITO: Denota los sectores de la nave.
-- COSTO: O(S).
    -- Siendo S la cantidad de Sectores, se utiliza la operación "domM" de costo "S". Es por eso que el costo total de la función es "S".
sectores (MKN msst _ _) = domM msst

----------------------------------------------------------------------------------------------------------------------------------------

conMayorRango :: Nave -> Tripulante
-- PROPÓSITO: Denota el tripulante con mayor rango.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utilizó la operación "maxH" de costo constante.
conMayorRango (MKN _ mht _) = maxH mht

----------------------------------------------------------------------------------------------------------------------------------------

conMasTripulantes :: Nave -> Sector
-- PROPÓSITO: Denota el Sector de la nave con más tripulantes.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utilizó la operación "fst" de costo constante.
conMasTripulantes (MKN _ _ si) = fst si

----------------------------------------------------------------------------------------------------------------------------------------

conRango :: Rango -> Nave -> Set Tripulante
-- PROPÓSITO: Denota el conjunto de tripulantes con dicho rango.
-- COSTO: O(P log P).
    -- Siendo P la cantidad de Tripulantes, en P se utiliza la función "conRangoMHT" de costo "P log P", es por eso que el
    -- costo total de la función es "P log P".
conRango r (MKN _ mht _) = conRangoMHT r mht

conRangoMHT :: Rango -> MaxHeap Tripulante -> Set Tripulante
-- PROPÓSITO: Denota el conjunto de tripulantes con el rango dado.
-- COSTO: O(P log P).
    -- Siendo P la cantidad de Tripulantes, por cada S se realizan las operaciones "addS" y "deleteMaxH" de costo "log P", además de
    -- las operaciones "isEmptyH", "emptyS" y "findMaxH", todas de costo constante. Es por eso que el costo total de la función es "P log P".
conRangoMHT r mht = if isEmptyH mht
                       then emptyS
                       else if rango (findMaxH mht) == r
                               then addS (findMaxH mht) (conRangoMHT r (deleteMaxH mht))
                               else conRangoMHT r (deleteMaxH mht)

----------------------------------------------------------------------------------------------------------------------------------------

sectorDe :: Tripulante -> Nave -> Sector
-- PROPÓSITO: Devuelve el sector en el que se encuentra un tripulante.
-- PRECONDICIÓN: El tripulante debe existir en los sectores dados.
-- COSTO: O(S log S log P).
    -- Siendo S la cantidad de Sectores y P la cantidad de Tripulantes; se utiliza la función "sectorDeT" de costo "S log S log P",
    -- es por eso que el costo total de la función es "S log S log P".
sectorDe t (MKN msst _ _) = sectorDeT t msst (domM msst)

sectorDeT :: Tripulante -> Map Sector (Set Tripulante) -> [Sector] -> Sector
-- PROPÓSITO: Devuelve el sector en el que se encuentra el tripulante dado.
-- PRECONDICIÓN: El tripulante debe existir en los sectores dados.
-- COSTO: O(S log S log P).
    -- Siendo S la cantidad de Sectores y P la cantidad de Tripulantes; en S se realiza la operación "lookupM" de costo "log S",
    -- en P se realiza la operación "belongs" de costo "log P". Es por eso que el costo total de la función es "S log S log P".  
sectorDeT t msst []     = error "El tripulante no existe en los sectores dado."
sectorDeT t msst (s:ss) = if belongs t fromJust(lookupM s mmst)
                             then s
                             else sectorDeT t msst ss

----------------------------------------------------------------------------------------------------------------------------------------

agregarTripulante :: Tripulante -> Sector -> Nave -> Nave
-- PROPÓSITO: Agrega un tripulante a ese sector de la nave.
-- PRECONDICIÓN: El sector está en la nave y el tripulante no.
-- COSTO: O(S log S + log P).
    -- Siendo S la cantidad de Sectores, y P la cantidad de Tripulantes; en S se realizan las operaciones "assocM" y "lookupM", ambas
    -- de costo "log S" además de utilizar la función "sectorConMasTripulante" de costo "S log S". En P se realizan las operaciones 
    -- "addS" e "insertH" ambas de costo "log P". Es por eso que el costo total de la función es "S log S + log P".
agregarTripulante t s (MKN msst mht si) = let tsActualizados = addS t (fromJust(lookupM s msst));
                                              nMSST = assocM s tsActualizados msst;
                                              nMHT = insertH t mht;
                                              nSi = sectorConMasTripulantes nMSST (domM nMSST) 
                                           in MKN nMSST nMHT nSi

sectorConMasTripulantes :: Map Sector (Set Tripulante) -> [Sector] -> (Sector, Int)
-- PROPÓSITO: Devuelve una tupla donde la primer componente es el Sector con más Tripulantes, y la segunda la cantidad de Tripulantes del mismo.
-- PRECONDICIÓN: Debe existir al menos un Sector en el Map dado.
-- COSTO: O(S log S).
    -- Siendo S la cantidad de Sectores, y P la cantidad de Tripulantes; por cada S se realiza operación "lookupM" de costo "log S", 
    -- además en P se realiza la operación "sizeS" de costo constante. Es por eso que el costo total de la función es "S log S".
sectorConMasTripulantes msst []     = error "No hay sectores, es por eso que no es posible determinar el sector con más tripulantes."
sectorConMasTripulantes msst (s:ss) = let setT = fromJust (lookupM s msst)
                                          n = sizeS setT
                                          (s', n') = sectorConMasTripulantes msst ss
                                       in if n >= n' 
                                             then (s, n) 
                                             else (s', n')