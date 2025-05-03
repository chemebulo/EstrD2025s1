module Empresa
    (Empresa, consEmpresa, buscarPorCUIL, empleadosDelSector, todosLosCUIL, todosLosSectores, 
              agregarSector, agregarEmpleado, agregarASector, borrarEmpleado)
where

import Set
import Map

type SectorId = Int
type CUIL = Int
--                   (Map   CUIL      Empleado   )
data Empresa = ConsE (Map SectorId (Set Empleado))
    deriving Show
{- INV. REP.:
    * 
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

-- #################################################### IMPLEMENTACIÓN ####################################################

consEmpresa :: Empresa
-- PROP: Construye una empresa vacía.
    -- COSTO: O(1).
    -- Siendo ...
consEmpresa = undefined


buscarPorCUIL :: CUIL -> Empresa -> Empleado
-- PROP: Devuelve el empleado con dicho CUIL.
-- PRECOND: El CUIL es de un empleado de la empresa.
    -- COSTO: O(log E).
    -- Siendo ...
buscarPorCUIL = undefined


empleadosDelSector :: SectorId -> Empresa -> [Empleado]
-- PROP: Indica los empleados que trabajan en un sector dado.
    -- COSTO: O(log S + E).
    -- Siendo ...
empleadosDelSector = undefined


todosLosCUIL :: Empresa -> [CUIL]
-- PROP: Indica todos los CUIL de empleados de la empresa.
    -- COSTO: O(E).
    -- Siendo ...
todosLosCUIL = undefined


todosLosSectores :: Empresa -> [SectorId]
-- PROP: Indica todos los sectores de la empresa.
    -- COSTO: O(S).
    -- Siendo ...
todosLosSectores = undefined


agregarSector :: SectorId -> Empresa -> Empresa
-- PROP: Agrega un sector a la empresa, inicialmente sin empleados.
    -- COSTO: O(log S).
    -- Siendo ...
agregarSector = undefined


agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
-- PROP: Agrega un empleado a la empresa, que trabajará en dichos sectores y tendrá el CUIL dado.
    -- COSTO: ...
    -- Siendo ...
agregarEmpleado = undefined


agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
-- PROP: Agrega un sector al empleado con dicho CUIL.
    -- COSTO: ...
    -- Siendo ...
agregarASector = undefined


borrarEmpleado :: CUIL -> Empresa -> Empresa
-- PROP: Elimina al empleado que posee dicho CUIL.
    -- COSTO: ...
    -- Siendo ...
borrarEmpleado = undefined

-- #################################################### AUXILIARES ####################################################