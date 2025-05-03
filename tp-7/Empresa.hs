module Empresa
    (Empresa, consEmpresa, buscarPorCUIL, empleadosDelSector, todosLosCUIL, todosLosSectores, 
              agregarSector, agregarEmpleado, agregarASector, borrarEmpleado)
where

import Set
import Map

type SectorId = Int
type CUIL     = Int
--                   
data Empresa = ConsE (Map SectorId (Set Empleado))
                     (Map CUIL Empleado)
    deriving Show
{- INV. REP.:
    * Siendo ConsE m1 m2: en m1 solamente se pueden relacionar ID de sectores con empleados que trabajan en dicho sector.
    * Siendo ConsE m1 m2: en m1 cada empleado puede estar asignado a más de un sector.
    * Siendo ConsE m1 m2: en m2 solamente se pueden relacionar un número CUIL con el empleado al que le pertenece.
-}

{- COSTO OPERACIONAL DE CADA FUNCIÓN:

- consEmpresa          O(1)
- buscarPorCUIL        O(log E)
- empleadosDelSector   O(log S + E)
- todosLosCUIL         O(E)
- todosLosSectores     O(S)
- agregarSector        O(log S)
- agregarEmpleado      O(...)
- agregarASector       O(...)
- borrarEmpleado       O(...)

-}

{- INTERFAZ DE EMPLEADO:

    consEmpleado :: CUIL -> Empleado
        -- PROP: Construye un empleado con dicho CUIL.
        -- COSTO: O(1).

    cuil :: Empleado -> CUIL
        -- PROP: Indica el CUIL de un empleado.
        -- COSTO: O(1).

    incorporarSector :: SectorId -> Empleado -> Empleado
        -- PROP: Incorpora un sector al conjunto de sectores en los que trabaja un empleado.
        -- COSTO: O(log S), siendo S la cantidad de sectores que el empleado tiene asignados.

    sectores :: Empleado -> [SectorId]
        -- PROP: Indica los sectores en los que el empleado trabaja.
        -- COSTO: O(S).
-}

{- 
    ------------------------------------------------
    |     INTERFACES DISPONIBLES COMO USUARIO      |
    |----------------------------------------------|
    |          MAP         |          SET          |
    |----------------------|-----------------------|
    |  emptyM    O(1)      |  emptyS     O(1)      |
    |  assocM    O(log n)  |  addS       O(log n)  |
    |  lookupM   O(log n)  |  belongs    O(log n)  |
    |  deleteM   O(log n)  |  sizeS      O(1)      |
    |  keys      O(n)      |  removeS    O(log n)  |
    |                      |  unionS     O(n)      |
    |                      |  setToList  O(n)      |
    ------------------------------------------------
-}

-- #################################################### IMPLEMENTACIÓN ####################################################

consEmpresa :: Empresa
-- PROP: Construye una empresa vacía.
    -- COSTO: O(1).
    -- Siendo de costo constante ya que lo único que hace es utilizar el constructor para construir una empresa vacía.
consEmpresa = ConsE emptyM emptyM


buscarPorCUIL :: CUIL -> Empresa -> Empleado
-- PROP: Devuelve el empleado con dicho CUIL.
-- PRECOND: El CUIL es de un empleado de la empresa.
    -- COSTO: O(log E).
    -- Siendo E la cantidad de empleados,
buscarPorCUIL c (ConsE m1 m2) = fromJust (lookupM c m2)


empleadosDelSector :: SectorId -> Empresa -> [Empleado]
-- PROP: Indica los empleados que trabajan en un sector dado.
    -- COSTO: O(log S + E).
    -- Siendo E la cantidad de empleados y S,
empleadosDelSector s (ConsE m1 m2) = let empleados = lookupM s m1
                                      in setToList empleados


todosLosCUIL :: Empresa -> [CUIL]
-- PROP: Indica todos los CUIL de empleados de la empresa.
    -- COSTO: O(E).
    -- Siendo E la cantidad de empleados,
todosLosCUIL (ConsE m1 m2) = keys m2


todosLosSectores :: Empresa -> [SectorId]
-- PROP: Indica todos los sectores de la empresa.
    -- COSTO: O(S).
    -- Siendo S...
todosLosSectores (ConsE m1 m2) = keys m1


agregarSector :: SectorId -> Empresa -> Empresa
-- PROP: Agrega un sector a la empresa, inicialmente sin empleados.
    -- COSTO: O(log S).
    -- Siendo ...
agregarSector s (ConsE m1 m2) = ConsE (assocM s emptyS m1) m2


agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
-- PROP: Agrega un empleado a la empresa, que trabajará en dichos sectores y tendrá el CUIL dado.
    -- COSTO: ...
    -- Siendo ...
agregarEmpleado ss c (ConsE m1 m2) = let emp = empleadoConSectores ss (consEmpleado c)
                                      in ConsE (agregarEmpleadoASectores emp ss m1) (assocM c emp m2)

empleadoConSectores :: [SectorId] -> Empleado -> Empleado
    -- COSTO: ...
    -- Siendo ...
empleadoConSectores []     e = e
empleadoConSectores (s:ss) e = empleadoConSectores ss (incorporarASector s e)

agregarEmpleadoASectores :: Empleado -> [SectorId] -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
    -- COSTO: ...
    -- Siendo ...
agregarEmpleadoASectores e []     m = m
agregarEmpleadoASectores e (s:ss) m = let se = fromJust (lookupM s m) 
                                       in if lookupM s m /= Nothing
                                             then agregarEmpleadoASectores e ss (assocM s (addS e se) m)
                                             else agregarEmpleadoASectores e ss m


agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
-- PROP: Agrega un sector al empleado con dicho CUIL.
    -- COSTO: ...
    -- Siendo ...
agregarASector s c (ConsE m1 m2) = let emp = incorporarSector s (fromJust (lookupM c m2))
                                    in ConsE (agregarEmpleadoASectores emp (s:(sectores emp)) m1) (assocM c emp m2)


borrarEmpleado :: CUIL -> Empresa -> Empresa
-- PROP: Elimina al empleado que posee dicho CUIL.
    -- COSTO: ...
    -- Siendo ...
borrarEmpleado c (ConsE m1 m2) = let emp = fromJust (lookupM c m2)
                                  in ConsE (actualizarSectoresDeLaEmpresa (sectores e) e m1) (deleteM c m2)

actualizarSectoresDeLaEmpresa :: [SectorId] -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
actualizarSectoresDeLaEmpresa []     e m = m 
actualizarSectoresDeLaEmpresa (s:ss) e m = let sinE = removeS e (fromJust (lookupM s msise))
                                            in if lookupM s m /= Nothing
                                                  then agregarEmpleadoASectores e ss (assocM s sinE m)
                                                  else agregarEmpleadoASectores e ss m

-- #################################################### AUXILIARES ####################################################

fromJust :: Maybe a -> a
    -- COSTO: O(1).
    -- Siendo x el dato de tipo "maybe a", devuelve x en el caso de que tenga el constructor "Just", entonces, eso implica 
    -- que el costo de todo el funcionamiento en el peor caso sea constante, ya que solo devuelve el dato. 
fromJust Nothing   = error "No tendria que dar esto."
fromJust (Just x)  = x