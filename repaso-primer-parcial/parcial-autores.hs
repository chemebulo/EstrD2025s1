-- EJERCICIO 1.A:

programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
-- PROPÓSITO: Dadas dos personas y un organizador, denota el conjunto de aquellos programas en las que las personas programaron juntas.
-- PRECONDICIÓN: Ambas personas deben existir en el organizador.
-- COSTO: O(P log P).
    -- Siendo P la cantidad de personas del organizador, en P se realiza la operación "programasDe" de costo "log P", además se utiliza
    -- la operación "intersectionS" de costo "P log P". Es por eso que el costo total de la función es "P log P".
programasEnComun p1 p2 org = let programasP1 = programasDe org p1;     -- log P
                                 programasP2 = programasDe org p2      -- log P
                              in intersectionS programasP1 programasP2 -- P log P

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 1.B:

esUnGranHacker :: Organizador -> Persona -> Bool
-- PROPÓSITO: Denota verdadero si la persona indicada aparece como autor de todos los programas del organizador.
-- COSTO: O(P + C^2).
    -- Siendo P la cantidad de personas, C la cantidad de programas del organizador; en P se utilizan las operaciones "programasDe"
    -- de costo "log P" y "setToList" de costo "P". Además, en C se realiza la operación "todosLosProgramas" de costo "C", y la función
    -- "mismosProgramas" de costo "C^2". Es por eso que el costo total de la función es "P + C^2".
esUnGranHacker org p = let programasP = setToList(programasDe p)   -- P
                           programasOrg = todosLosProgramas org    -- C
                        in mismosProgramas programasOrg programasP -- C^2

mismosProgramas :: [Checksum] -> [Checksum] -> Bool
-- PROPÓSITO: Describe si la primera lista de programas está incluida en la segunda lista de programas dada.
-- COSTO: O(C^2).
    -- Siendo C la cantidad de programas del organizador, por cada C se realiza la operación "elem" de costo "C", es por eso que el 
    -- costo total de la función es "C^2".
mismosProgramas []       _   = True
mismosProgramas (c1:cs1) cs2 = elem c1 cs2 && mismosProgramas cs1 cs2 

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 2.A:

data Organizador = MKO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))
    deriving Show

{- INV. REP.:
    * Siendo MK0 mcsp mpsc:
        * Cada Persona de cada Checksum en MCSP, tiene que existir como clave en MPSC y cada una tener como valor
          al menos a dicho Checksum.
        * Cada Checksum de cada Persona en MPSC, tiene que existir como clave en MCSP y cada una tener como valor
          al menos a dicha Persona.
-}

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 2.B:

nuevo :: Organizador
-- PROPÓSITO: Un organizador vacío.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza la operación "emptyM" de costo constante.
nuevo = MK0 emptyM emptyM

----------------------------------------------------------------------------------------------------------------------------------------

agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
-- PROPÓSITO: Agrega al organizaodr un programa con el Checksum indicado; el conjunto es el conjunto de las personas autores
--            de dicho programa.
-- PRECONDICIÓN: El identificador del programa que se agrega no fue usado previamente en el organizador, y el Set de personas
--               no está vacío.
-- COSTO: O(P log P + log C).
    -- Siendo P la cantidad de Personas, C la cantidad de Checksum; en P se realiza la operación "setToList" de costo "P", y se utiliza
    -- la función "asociar" de costo "P log P + log C". En C se realiza la operación "assocM" de costo "log C". Es por eso que el costo
    -- total de la función es "P log P + log C".
agregarPrograma (MK0 mcsp mpsc) c sp = let nMCSP = assocM c sp mcsp;              -- log C
                                           nMPSC = asociar (setToList sp) c mpsc; -- P log P + log C
                                        in MK0 nMCSP nMPSC

asociar :: [Persona] -> Checksum -> Map Persona (Set Checksum) -> Map Persona (Set Checksum) 
-- PROPÓSITO: Asocia todas las Personas de la lista con el Checksum dado al Map.
-- COSTO: O(P log P + log C).
    -- Siendo P la cantidad de personas, C la cantidad de Checksum; por cada P se realizan las operaciones "lookupM" y "assocM"
    -- de costo "log P". En C se realiza la operación "addS" de costo "log C". Es por eso que el costo total de la función es
    -- "P log P + log C".
asociar []     c mpsc = mpsc
asociar (p:ps) c mpsc = case lookupM p mpsc of
                          Just sc -> asociar ps c (assocM p (addS c sc) mpsc)
                          Nothing -> asociar ps c (assocM p (addS c emptyS) mpsc)

----------------------------------------------------------------------------------------------------------------------------------------

todosLosProgramas :: Organizador -> [Checksum]
-- PROPÓSITO: Denota una lista con todos y cada uno de los códigos identificadores de programas del organizador.
-- COSTO: O(C).
    -- Siendo C la cantidad de códigos en el organizador, se realiza la operación "domM" de costo "C", es por eso que el
    -- costo total de la función es "C".
todosLosProgramas (MK0 mcsp _) = domM mcsp

----------------------------------------------------------------------------------------------------------------------------------------

autoresDe :: Organizador -> Checksum -> Set Persona
-- PROPÓSITO: Denota el conjunto de autores que aparecen en un programa determinado.
-- PRECONDICIÓN: El Checksum debe corresponder a un programa del organizador.
-- COSTO: O(log C).
    -- Siendo C la cantidad total de Checksum del organizador, en C se realiza la operación "lookupM" de costo "log C", es por eso
    -- que el costo total de la función es "log C".
autoresDe (MK0 mcsp _) c = case lookupM c mcsp of
                             Just sp  -> sp
                             Nothingt -> error "El Checksum dado no corresponde a un programa del organizador."

----------------------------------------------------------------------------------------------------------------------------------------

programasDe :: Organizador -> Persona -> Set Checksum
-- PROPÓSITO: Denota el conjunto de programas en los que participó una determinada persona.
-- PRECONDICIÓN: La persona debe existir en el organizador.
-- COSTO: O(log P).
    -- Siendo P la cantidad total de Personas del organizador, en P se realiza la operación "lookupM" de costo "log P", es por eso
    -- que el costo total de la función es "log P".
programasDe (MK0 _ mpsc) p = case lookupM p mpsc of
                               Just sc -> sc
                               Nothing -> error "La Persona dada no existe en el organizador." 

----------------------------------------------------------------------------------------------------------------------------------------

programaronJuntas :: Organizador -> Persona -> Persona -> Bool
-- PROPÓSITO: Dado un organizador y dos personas, denota verdadero si ambas son autores de algún software en común.
-- PRECONDICIÓN: Las personas deben ser distintas.
-- COSTO: O(log P + C log C).
    -- Siendo P la cantidad de personas distintas que aparecen en todos los programas del organizador y C la cantidad total de programas;
    -- se utiliza la función "programaronJuntasMCSP" de costo "log P + C log C", es por eso que el costo total de la función es ese.
programaronJuntas (MK0 mcsp _) p1 p2 = programaronJuntasMCSP mcsp (domM mcsp) p1 p2  

programaronJuntasMCSP :: Map Checksum (Set Persona) -> [Checksum] -> Persona -> Persona -> Bool
-- PROPÓSITO: Dado un Map, claves del Map y dos personas, denota verdadero si ambas son autores de algún software en común.
-- PRECONDICIÓN: Las personas deben ser distintas.
-- COSTO: O(log P + C log C).
    -- Siendo P la cantidad de personas distintas que aparecen en todos los programas del organizador y C la cantidad total de programas;
    -- en P se realiza la operación "belongs" de costo "log P", por cada C se realiza la operación "lookupM" de costo "log C".
    -- Es por eso que el costo total de la función es "log P + C log C".
programaronJuntasMCSP mcsp []     p1 p2 = False
programaronJuntasMCSP mcsp (c:cs) p1 p2 = case lookupM c mcsp of
                                            Just sp -> (belongs p1 sp && belongs p2 sp) || programaronJuntasMCSP mcsp cs p1 p2
                                            Nothing -> error "No debería dar este error."

----------------------------------------------------------------------------------------------------------------------------------------

nroProgramasDePersona :: Organizador -> Persona -> Int
-- PROPÓSITO: Dado un organizador y una persona, denota la cantidad de programas distintos en los que aparece.
-- PRECONDICIÓN: La persona debe existir en el organizador.
-- COSTO: O(log P).
    -- Siendo P la cantidad total de Personas del organizador, en P se realiza la operación "lookupM" de costo "log P", es por eso
    -- que el costo total de la función es "log P". También se utiliza la operación "sizeS" pero es de costo constante.
nroProgramasDePersona (MK0 _ mpsc) p = case lookupM p mpsc of
                                         Just sc -> sizeS sc
                                         Nothing -> error "La Persona dada no existe en el organizador." 

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 3:

elMayorPrograma :: Organizador -> Maybe Checksum
-- PROPÓSITO: Recibe un organizador y denota uno de los programas con más autores de todo ese organizador; denota Nothing 
--            si no puede devolver un programa.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza la operación "maxH" de costo constante.
elMayorPrograma (MK0 mcsp mpsc mhc) = maxH mhc  

----------------------------------------------------------------------------------------------------------------------------------------
-- Para que el Ejercicio 3 sea posible, requirió los siguientes cambios:

data Organizador = MKO (Map Checksum (Set Persona)) (Map Persona (Set Checksum)) MaxHeap Checksum
    deriving Show

{- INV. REP.:
    * Siendo MK0 mcsp mpsc mhc:
        * Cada Persona de cada Checksum en MCSP, tiene que existir como clave en MPSC y cada una tener como valor
          al menos a dicho Checksum.
        * Cada Checksum de cada Persona en MPSC, tiene que existir como clave en MCSP y cada una tener como valor
          al menos a dicha Persona.
        * Cada Checksum en MCSP debe existir también en MHC.
-}

----------------------------------------------------------------------------------------------------------------------------------------

nuevo :: Organizador
-- PROPÓSITO: Un organizador vacío.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utilizan las operaciones "emptyM" y "emptyH", ambas de costo constante.
nuevo = MK0 emptyM emptyM emptyH

----------------------------------------------------------------------------------------------------------------------------------------

agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
-- PROPÓSITO: Agrega al organizaodr un programa con el Checksum indicado; el conjunto es el conjunto de las personas autores
--            de dicho programa.
-- PRECONDICIÓN: El identificador del programa que se agrega no fue usado previamente en el organizador, y el Set de personas
--               no está vacío.
-- COSTO: O(P log P + log C).
    -- Siendo P la cantidad de Personas, C la cantidad de Checksum; en P se realiza la operación "setToList" de costo "P", y se utiliza
    -- la función "asociar" de costo "P log P + log C". En C se realiza la operación "assocM" e "insertH" ambas de costo "log C". 
    -- Es por eso que el costo total de la función es "P log P + log C".
agregarPrograma (MK0 mcsp mpsc mhc) c sp = let nMCSP = assocM c sp mcsp;              -- log C
                                               nMPSC = asociar (setToList sp) c mpsc; -- P log P + log C
                                               nMHC = insertH c mhc                   -- log C
                                            in MK0 nMCSP nMPSC nMHC

