-- 12:15

-- type Esmeralda = Int
-- type 

-- EJERCICIO 1:

partida :: [Comando] -> Personaje
-- PROPÓSITO: Dada una lista de comandos valida, describe el ganador del juego que 
--            resulta luego de ejecutar la lista.
-- PRECONDICIÓN: La lista de comandos debe ser válida.
-- OBSERVACIONES: Una lista de comandos válida es aquella que su primer elemento es IniciarJuego y los 
--                diferentes comandos cumplen las precondiciones de los omandos que representan al momento
--                de ser ejecutados. 
-- COSTO: O(C * (P log P + E) + E log E).
    -- Siendo P la cantidad de personajes en el juego, E la cantidad de esmeraldas y C la cantidad de comandos; en
    -- cada C se ejecuta la función "ejecutarComandos" de costo (C * (P log P + E)) en el peor caso. Es por eso que el 
    -- costo total es ese, ya que el comando "elMasPoderoso" es de costo constante. Además, se ejecuta "iniciarJuego"
    -- de costo "E log E" una sola vez.
partida cs = elMasPoderoso $ ejecutarComandos $ reverse cs

ejecutarComandos :: [Comando] -> EmGame
-- PROPÓSITO: Dado una lista de comandos, describe el juego después de ejecutar cada uno de ellos.
-- PRECONDICIÓN: La lista de comandos debe ser válida.
-- OBSERVACIONES: Una lista de comandos válida es aquella que su primer elemento es IniciarJuego y los 
--                diferentes comandos cumplen las precondiciones de los omandos que representan al momento
--                de ser ejecutados.  
-- COSTO: O(C * (P log P + E)).
    -- Siendo P la cantidad de personajes en el juego, E la cantidad de esmeraldas y C la cantidad de comandos; por
    -- cada C se ejecuta la función "ejecutar" de costo (P log P + E) en el peor caso. Es por eso que el costo total
    -- de la función es (C * P log P + E), ya que por cada comando ese es el peor caso.
ejecutarComandos []     = error "Lista de comandos inválida."
ejecutarComandos [c]    = case c of
                            IniciarJuego sp se -> IniciarJuego sp se
                            _                  -> error "Lista de comandos inválida."
ejecutarComandos (c:cs) = ejecutar c (ejecutarComandos cs)

ejecutar :: Comando -> EmGame -> EmGame
-- PROPÓSITO: Dado un comando y una situación del juego, describe el juego después de ejecutar dicho comando. 
-- PRECONDICIONES: Ninguna.
-- COSTO: O(P log P + E).
    -- Siendo P la cantidad de personajes en el juego y E la cantidad de esmeraldas, en el peor de los casos
    -- se ejecuta el comando "usarEsmeralda" de costo (P log P + E).
ejecutar (IniciarJuego _ _)       eg = error "Lista de comandos inválida."
ejecutar (ObtenerEsmeralda sp se) eg = obtenerEsmeralda eg sp se
ejecutar (CompetirPor p1 p2 e)    eg = ganarEsmeralda eg p1 p2 e
ejecutar (UsarEsmeralda p e)      eg = usarEsmeralda eg p e


-- EJERCICIO 2:

data EmGame = AG (Map Personaje [Esmeralda])
                 (Map Esmeralda (Maybe Personaje))
                 (MaxHeap Personaje)
{- INV. REP.:
    * Siendo AG mpe memp mhp:
        * Todos los personajes que estan en MPE deben estar también en MHP, y si tienen esmeraldas en MEMP. 
        * Todas las esmeraldas asociadas a personajes en MPE deben estar asociadas en MEMP, es decir, en MEMP
          cada esmeralda debe estar asociada al personaje que tiene asociado en MPE.
        * Todos los personajes asociados a una esmeralda en MEMP, deben estar asociados en MPE, es decir, en MPE
          cada personaje tiene que tener asociadado todas las esmeraldas que tiene asociado en MEMP.
        * Las esmeraldas que tiene cada personaje en MPE son únicas, es decir, solo ellos la poseen. Además,
          no pueden tener la misma esmeralda dos veces.
        * En MHP no hay personajes repetidos.
-}


-- EJECICIO 3:

iniciarJuego :: Set Personaje → Set Esmeralda → EmGame 
-- PROPÓSITO: Dado un conjunto de personajes y un conjunto de esmeraldas (suponiendo ambos no vacios), describe 
--            un juego inicial con esas esmeraldas y esos personajes sin esmeraldas. 
-- PRECONDICIONES: Ambos set no estan vacíos.
-- COSTO: O(P log P + E log E).
    -- Siendo P la cantidad de personajes en el juego y E la cantidad de esmeraldas; se realiza la operación "setToList"
    -- en el Set de Personaje y Set de Esmeralda, dicha operación de costo "P" y "E" respectivamente. También, se utiliza
    -- la función "asociarPersonajes" de costo (P log P), la función "asociarEsmeraldas" de costo (E log E), y la función
    -- "insertarPersonajes" de costo (P log P). Esto resulta con que el costo total de la función sea (P log P + E log E).
iniciarJuego sp se = let personajes = setToList sp;
                         esmeraldas = setToList se;
                         nMPE = asociarPersonajes personajes;
                         nMEMP = asociarEsmeraldas esmeraldas;
                         nMHP = insertarPersonajes personajes
                      in AG nMPE nMEMP nMHP

asociarPersonajes :: [Personaje] -> Map Personaje [Esmeralda]
-- PROPÓSITO: Dado una lista de personajes, asocia cada personaje sin esmeraldas a un Map.
-- COSTO: O(P log P).
    -- Siendo P la cantidad de personajes en el juego y E la cantidad de esmeraldas; por cada P se realiza
    -- la operación "assocM" de costo (log P), es por eso que el costo total de la función es "P log P"
asociarPersonajes []     = emptyM
asociarPersonajes (p:ps) = assocM p [] (asociarPersonajes ps)


asociarEsmeraldas :: [Esmeralda] -> Map Personaje [Esmeralda]
-- PROPÓSITO: Dado una lista de esmeraldas, asocia cada esmeralda sin un personaje asociado a un Map.
-- COSTO: O(E log E).
    -- Siendo P la cantidad de personajes en el juego y E la cantidad de esmeraldas; por cada E se realiza
    -- la operación "assocM" de costo (log E), es por eso que el costo total de la función es "E log E".
asociarEsmeraldas []     = emptyM
asociarEsmeraldas (e:es) = assocM e Nothing (asociarEsmeraldas es)

insertarPersonajes :: [Personaje] -> MaxHeap Personaje
-- PROPÓSITO: Dado una lista de personajes, inserta cada personaje a un MaxHeap.
-- COSTO: O(P log P).
    -- Siendo P la cantidad de personajes en el juego y E la cantidad de esmeraldas; por cada P se realiza
    -- la operación "insertH" de costo (log P), es por eso que el costo total de la función es "P log P".
insertarPersonajes []     = emptyH
insertarPersonajes (p:ps) = insertH p (insertarPersonajes ps)

----------------------------------------------------------------------------------------------------------------------------------------

elMasPoderoso :: EmGame -> Personaje
-- PROPÓSITO: Dado un juego describe el mas poderoso.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza la operación "maxH" (de costo constante) en la MaxHeap.
elMasPoderoso (AG _ _ mhp) = maxH mhp

----------------------------------------------------------------------------------------------------------------------------------------

esmeraldasDe :: EmGame → Personaje → [Esmeralda] 
-- PROPÓSITO: Dado un juego y un personaje, describe la lista de esmeraldas de ese personaje. 
-- PRECONDICIONES: El personaje debe estar en el juego. 
-- COSTO: O(log P).
    -- Siendo P la cantidad de personajes en el juego, se realiza la operación "lookupM" de costo (log P), es por eso
    -- que el costo total de la función es ese. 
esmeraldasDe (AG mpe _ _) p = case lookupM p mpe of
                                Just es -> es
                                Nothing -> error "El personaje no está en el juego."

----------------------------------------------------------------------------------------------------------------------------------------

obtenerEsmeralda :: EmGame → Personaje → Esmeralda → EmGame 
-- PROPÓSITO: Dado un juego, personaje y esmeralda dentro de ese juego, describe el resultado de 
--            asignar la esmeralda al personaje suponiendo que esa esmeralda no esta asignada. 
-- PRECONDICIONES: El personaje y la esmeralda deben estar en el juego.
-- COSTO: O (log P + log E).
    -- Siendo P la cantidad de personajes en el juego y E la cantidad de esmeraldas; se utiliza la operación "lookupM" 
    -- y "assocM" varias veces, siendo de costo (log P) y (log E) dependiendo en sobre en qué se utilice. Es por eso que el costo
    -- total de la función es (log P + log E), ya que en promedio ese es el costo de utilizar todas las operaciones que se utilizaron. 
obtenerEsmeralda (AG mpe memp mhp) p e = case lookupM p mpe of
                                           Nothing -> error "El personaje no existe en el juego." 
                                           Just es -> case lookupM e memp of
                                                        Nothing -> error "La esmeralda no existe en el juego."
                                                        Just p' -> let nMPE = (assocM p (e:es) mpe);
                                                                       nMEMP = (assocM e (Just p) memp);
                                                                    in case p' of
                                                                         Nothing -> AG nMPE nMEMP mhp
                                                                         Just _  -> error "La esmeralda está asignada a otro personaje."

----------------------------------------------------------------------------------------------------------------------------------------

ganarEsmeralda :: EmGame → Personaje → Personaje → Esmeralda 
-- PROPÓSITO: Dado un juego, dos personajes y una esmeralda dentro de ese juego, describe el resultado de realizar la competencia 
--            entre ambos personajes, suponiendo que esa esmeralda la tiene alguno de los personajes, y se la queda el mas poderoso.
-- PRECONDICIONES: Cada personaje y la esmeralda deben estar en el juego, y la esmeralda la debe tener alguno de los dos personajes.
-- COSTO: O (log P + E).
    -- Siendo P la cantidad de personajes en el juego y E la cantidad de esmeraldas; se utiliza varias veces la operación "assocM",
    -- "lookupM", de costo (log E) y (log P) respectivamente. También se utiliza la función "elem" de costo (E), la función "transferir"
    -- de costo (log P + E). Es por eso que el costo total de la función es (log P + E), ya que la función / operación más pesada es esa.
ganarEsmeralda (AG mpe memp mhp) p1 p2 e = 
    case (lookupM p1 mpe, lookupM p2 mpe, lookupM e memp) of
      (Just es1, Just es2, Just p') -> if not (elem e es1 || elem e es2)
                                          then error "La esmeralda no la tiene ninguno de los dos personajes."
                                          else if poder p1 >= poder p2 
                                                  then if elem e es1
                                                          then AG mpe memp mhp
                                                          else AG (transferir p1 p2 es1 es2 e mpe)
                                                                  (assocM e (Just p1) memp)
                                                                  mph
                                                  else if elem e es2
                                                          then AG mpe memp mhp
                                                          else AG (transferir p2 p1 es2 es1 e mpe)
                                                                  (assocM e (Just p1) memp)
                                                                  mph
      _                             -> error "Algunos de los personajes o la esmeralda no existen en el juego."

transferir :: Personaje -> Personaje -> [Esmeralda] -> [Esmeralda] -> Esmeralda -> Map Personaje [Esmeralda] -> Map Personaje [Esmeralda]
-- PROPÓSITO: Dado dos personajes, dos listas de esmeraldas que corresponden a cada personaje respectivamente, una esmeralda, y un map,
--            describe el map resultante de transferir la esmeralda en el primer personaje al segundo.
-- COSTO: O(log P + E).
    -- Siendo P la cantidad de personajes en el juego y E la cantidad de esmeraldas; se utiliza la operación "assocM" de costo
    -- (log P) y también se utiliza la función "remove" de costo (E). Es por eso que el costo total de la función es (log P + E).
transferir p1 p2 es1 es2 e mpe = assocM p2 (e:es2) (assocM p1 (remove es1 e) mpe)

remove :: Eq a => [a] -> a -> [a]
-- PROPÓSITO: Dado una lista de elementos y un elemento, describe la lista resultante de quitar el elemento de la lista dada.
-- COSTO: O(N).
    -- Siendo N la cantidad de elementos, por cada N se realiza una operación de costo constante (==), es por eso que el costo total
    -- de la función en el peor caso es O(N), lineal.  
remove []     y = []
remove (x:xs) y = if x == y
                     then xs
                     else x:(removeE xs y)

----------------------------------------------------------------------------------------------------------------------------------------

usarEsmeralda :: EmGame → Personaje → Esmeralda → EmGame 
-- PROPÓSITO: Dado un juego, personaje, esmeralda (dentro de ese juego), describe el juego que resulta de la ultilizacion
--            de su esmeralda por el personaje, suponiendo que el personaje tiene la esmeralda. 
-- PRECONDICIÓN: El personaje y la esmeralda deben estar en el juego, y además, el personaje debe tener dicha esmeralda.
-- COSTO: O(log P + E).
    -- Siendo P la cantidad de personajes en el juego y E la cantidad de esmeraldas; se utilizan las operaciones "lookupM",
    -- "assocM", "deleteM" e "insertH" costo "log P"; también las operaciones "aumentarPoder" y "poder" de costo constante.
    -- A su vez, se utilizan las funciones "elem" y "remove" de costo "E". Es por eso que el costo total de la función es
    -- (log P + E) en el peor caso.
usarEsmeralda (AG mpe memp mhp) p e = case lookupM p mpe of
                                        Nothing -> error "El personaje no existe en el juego."
                                        Just es -> if not (elem e es)
                                                      then error "El personaje no tiene la esmeralda dada."
                                                      else let p' = aumentarPoder p (poder p) 
                                                            in AG (assocM p' (remove e es) (deleteM p mpe))
                                                                  (deleteM e memp)
                                                                  (insertH p' (removeH p mhp))

removeH :: Ord a => a -> Heap a -> Heap a
-- PROPÓSITO: Dado un elemento y una Heap, describe la Heap resultante de quitar el elemento dado en la misma.
-- COSTO: O(log N).
    -- Siendo N la cantidad de elementos en la Heap, por cada N se realizan las operaciones "isEmptyH", "maxH", ambas
    -- de costo constante. También realiza las operaciones "deleteMaxH" e "insertH", ambas de costo "log N". En el peor
    -- de los casos, el costo total de la función es "log N".
removeH x h = if isEmptyH h
                 then h
                 else let y = maxH h
                       in if x == y
                             then deleteMaxH h
                             else insertH y (removeH x (deleteMaxH h))