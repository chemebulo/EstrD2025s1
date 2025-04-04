-- PUNTO 1: Recursión sobre listas.

-- EJERCICIO 1.1:

sumatoria :: [Int] -> Int
-- PRECOND: Ninguna.
sumatoria []     = 0
sumatoria (n:ns) = n + sumatoria ns


-- EJERCICIO 1.2:

longitud :: [a] -> Int
-- PRECOND: Ninguna.
longitud []     = 0
longitud (x:xs) = 1 + longitud xs


-- EJERCICIO 1.3:

sucesores :: [Int] -> [Int]
-- PRECOND: Ninguna.
sucesores []     = []
sucesores (n:ns) = n+1 : sucesores ns


-- EJERCICIO 1.4:

conjuncion :: [Bool] -> Bool
-- PRECOND: Ninguna.
conjuncion []     = True
conjuncion (b:bs) = b && conjuncion bs


-- EJERCICIO 1.5:

disyuncion :: [Bool] -> Bool
-- PRECOND: Ninguna.
disyuncion []     = False
disyuncion (b:bs) = b || disyuncion bs


-- EJERCICIO 1.6:

aplanar :: [[a]] -> [a]
-- PRECOND: Ninguna.
aplanar []     = []
aplanar (x:xs) = x ++ aplanar xs


-- EJERCICIO 1.7:

pertenece :: Eq a => a -> [a] -> Bool
-- PRECOND: Ninguna.
pertenece e []     = False
pertenece e (x:xs) = e == x || pertenece e xs


-- EJERCICIO 1.8:

apariciones :: Eq a => a -> [a] -> Int
-- PRECOND: Ninguna.
apariciones e []     = 0
apariciones e (x:xs) = unoSi (esMismoElementoA e x) + apariciones e xs

unoSi :: Bool -> Int
-- PRECOND: Ninguna.
unoSi True  = 1
unoSi False = 0

esMismoElementoA :: Eq a => a -> a -> Bool
-- PRECOND: Ninguna.
esMismoElementoA x y = x == y


-- EJERCICIO 1.9:

losMenoresA :: Int -> [Int] -> [Int]
-- PRECOND: Ninguna.
losMenoresA n []     = []
losMenoresA n (ns:nss) = listaDeNumeroSiSinoNil ns (ns < n) n ++ losMenoresA n nss

listaDeNumeroSiSinoNil :: Int -> Bool -> Int -> [Int]
-- PRECOND: Ninguna.
listaDeNumeroSiSinoNil ns True  n = [ns]
listaDeNumeroSiSinoNil ns False n = []


-- EJERCICIO 1.10:

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
-- PRECOND: El número es mayor o igual a 0.
lasDeLongitudMayorA n []     = []
lasDeLongitudMayorA n (x:xs) = listaDeElementoSiSinoNil x (longitud x > n) ++ lasDeLongitudMayorA n xs
    
listaDeElementoSiSinoNil :: [a] -> Bool -> [[a]]
-- PRECOND: Ninguna.
listaDeElementoSiSinoNil x True  = [x]
listaDeElementoSiSinoNil x False = []
                                  


-- EJERCICIO 1.11:

agregarAlFinal :: [a] -> a -> [a]
-- PRECOND: Ninguna.
agregarAlFinal []     y = [y]
agregarAlFinal (x:xs) y = x : agregarAlFinal xs y


-- EJERCICIO 1.12:

agregar :: [a] -> [a] -> [a]
-- PRECOND: Ninguna.
agregar []     ys = ys
agregar (x:xs) ys = x : agregar xs ys


-- EJERCICIO 1.13:

reversa :: [a] -> [a]
-- PRECOND: Ninguna.
reversa []     = []
reversa (x:xs) = agregar (reversa xs) [x]


-- EJERCICIO 1.14:

zipMaximos :: [Int] -> [Int] -> [Int]
-- PRECOND: Ninguna.
zipMaximos []     ys     = ys
zipMaximos xs     []     = xs
zipMaximos (x:xs) (y:ys) = primerNumeroSiSinoElSegundo x (x >= y) y : zipMaximos xs ys

primerNumeroSiSinoElSegundo :: Int -> Bool -> Int -> Int
-- PRECOND: Ninguna.
primerNumeroSiSinoElSegundo n True  m = n
primerNumeroSiSinoElSegundo n False m = m


-- EJERCICIO 1.15 V1:

elMinimo :: Ord a => [a] -> a
-- PRECOND: La lista no es vacía.
elMinimo [x]    = x
elMinimo (x:xs) = min x (elMinimo xs)


-- EJERCICIO 1.15 V2:

{-

elMinimo :: Ord a => [a] -> a
-- PRECOND: La lista no es vacía.
elMinimo [x]    = x
elMinimo (x:xs) = elementoSiSino x (x < elMinimo xs) (elMinimo xs)

elementoSiSino :: a -> Bool -> a -> a 
-- PRECOND: Ninguna.
elementoSiSino x True  y = x
elementoSiSino x False y = y

-}

-- PUNTO 2: Recusión sobre números.


-- EJERCICIO 2.1:

factorial :: Int -> Int
-- PRECOND: El número es mayor o igual a 0.
factorial 0 = 1
factorial n = n * factorial (n-1)


-- EJERCICIO 2.2:

cuentaRegresiva :: Int -> [Int]
-- PRECOND: El número es mayor o igual a 0.
cuentaRegresiva n = if n >= 1
                       then n : cuentaRegresiva (n-1)
                       else [] 


-- EJERCICIO 2.3:

repetir :: Int -> a -> [a]
-- PRECOND: El número es mayor o igual a 0.
repetir 0 e = []
repetir n e = e : repetir (n-1) e


-- EJERCICIO 2.4:

losPrimeros :: Int -> [a] -> [a]
-- PRECOND: El número es mayor o igual a 0.
losPrimeros 0 _      = []
losPrimeros _ []     = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs


-- EJERCICIO 2.5:

sinLosPrimeros :: Int -> [a] -> [a]
-- PRECOND: El número es mayor o igual a 0.
sinLosPrimeros 0 xs     = xs
sinLosPrimeros _ []     = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs


-- PUNTO 3: Registros.


-- EJERCICIO 3.1:

data Persona = ConsPersona String Int
--                         Nombre Edad
{- INV. REP:
    - El nombre no es vacío.
    - La edad tiene un número mayor o igual a cero. 
-}
    deriving Show


mayoresA :: Int -> [Persona] -> [Persona]
-- PRECOND: El número es mayor o igual a cero.
mayoresA 0 _      = []
mayoresA _ []     = []
mayoresA n (x:xs) = listaDePersonaSiSinoNil x (edad x > n) ++ mayoresA n xs

listaDePersonaSiSinoNil :: Persona -> Bool -> [Persona]
-- PRECOND: Ninguna.
listaDePersonaSiSinoNil p True  = [p]
listaDePersonaSiSinoNil p False = []

edad :: Persona -> Int
-- PRECOND: Ninguna.
edad (ConsPersona n e) = e



promedioEdad :: [Persona] -> Int
-- PRECOND: La lista al menos posee una persona.
promedioEdad xs = div (sumatoriaDeEdades xs) (longitud xs)

sumatoriaDeEdades :: [Persona] -> Int
-- PRECOND: Ninguna.
sumatoriaDeEdades []     = 0
sumatoriaDeEdades (x:xs) = edad x + sumatoriaDeEdades xs



elMasViejo :: [Persona] -> Persona
-- PRECOND: La lista al menos posee una persona.
elMasViejo [x]    = x
elMasViejo (x:xs) = personaSiSino x (edad x > edad (elMasViejo xs)) (elMasViejo xs)

personaSiSino :: Persona -> Bool -> Persona -> Persona
-- PRECOND: Ninguna.
personaSiSino p1 True  p2 = p1
personaSiSino p1 False p2 = p2


-- EJERCICIO 3.2:

data TipoDePokemon = Agua | Fuego | Planta
    deriving Show

data Pokemon = ConsPokemon TipoDePokemon Int
--                                       Porcentaje de energía.
{- INV. REP:
    - El porcentaje de energía tiene un número mayor o igual a cero. 
-}
    deriving Show

data Entrenador = ConsEntrenador String [Pokemon]
--                               Nombre 
{- INV. REP:
    - El nombre no es vacío.
-}
    deriving Show


cantPokemon :: Entrenador -> Int
-- PRECOND: Ninguna.
cantPokemon (ConsEntrenador n p) = sumatoriaDePokemones p

sumatoriaDePokemones :: [Pokemon] -> Int
-- PRECOND: Ninguna.
sumatoriaDePokemones []     = 0
sumatoriaDePokemones (p:ps) = 1 + sumatoriaDePokemones ps



cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
-- PRECOND: Ninguna.
cantPokemonDe t (ConsEntrenador n p) = sumatoriaDePokemonesDe t p

sumatoriaDePokemonesDe :: TipoDePokemon -> [Pokemon] -> Int
-- PRECOND: Ninguna
sumatoriaDePokemonesDe t []     = 0
sumatoriaDePokemonesDe t (p:ps) = unoSiPokemonEsTipoCeroSino t p + sumatoriaDePokemonesDe t ps

unoSiPokemonEsTipoCeroSino :: TipoDePokemon -> Pokemon -> Int
-- PRECOND: Ninguna.
unoSiPokemonEsTipoCeroSino t (ConsPokemon tp e)  = unoSiTipoEsMismoTipoQue t tp

unoSiTipoEsMismoTipoQue :: TipoDePokemon -> TipoDePokemon -> Int
-- PRECOND: Ninguna.
unoSiTipoEsMismoTipoQue Agua   Agua   = 1
unoSiTipoEsMismoTipoQue Fuego  Fuego  = 1
unoSiTipoEsMismoTipoQue Planta Planta = 1
unoSiTipoEsMismoTipoQue _      _      = 0



cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
-- PRECOND: Ninguna.
cuantosDeTipo_De_LeGananATodosLosDe_ tp (ConsEntrenador n1 p1) (ConsEntrenador n2 p2) = nroDePokemonDeTipoQueLeGanarianALosDe tp p1 p2

nroDePokemonDeTipoQueLeGanarianALosDe :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
-- PRECOND: Ninguna.
nroDePokemonDeTipoQueLeGanarianALosDe tp []     _  = 0
nroDePokemonDeTipoQueLeGanarianALosDe tp _      [] = 0
nroDePokemonDeTipoQueLeGanarianALosDe tp (x:xs) ys = if esDelMismoTipo tp (tipoDePoke x) && leGanaATodos x ys
                                                        then 1 + nroDePokemonDeTipoQueLeGanarianALosDe tp xs ys
                                                        else nroDePokemonDeTipoQueLeGanarianALosDe tp xs ys

leGanaATodos :: Pokemon -> [Pokemon] -> Bool
-- PRECOND: Ninguna.
leGanaATodos p []     = True
leGanaATodos p (x:xs) = tipoDeEsSuperiorQue (tipoDePoke p) (tipoDePoke x) && leGanaATodos p xs

tipoDeEsSuperiorQue :: TipoDePokemon -> TipoDePokemon -> Bool
-- PRECOND: Ninguna.
tipoDeEsSuperiorQue Agua   Fuego  = True
tipoDeEsSuperiorQue Fuego  Planta = True
tipoDeEsSuperiorQue Planta Agua   = True
tipoDeEsSuperiorQue _      _      = False

esDelMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
-- PRECOND: Ninguna.
esDelMismoTipo Agua   Agua   = True
esDelMismoTipo Fuego  Fuego  = True
esDelMismoTipo Planta Planta = True
esDelMismoTipo _      _      = False

tipoDePoke :: Pokemon -> TipoDePokemon
-- PRECOND: Ninguna.
tipoDePoke (ConsPokemon t p) = t



esMaestroPokemon :: Entrenador -> Bool
-- PRECOND: Ninguna.
esMaestroPokemon (ConsEntrenador n p) = tieneAlMenosUnPokemonDeCada p

tieneAlMenosUnPokemonDeCada :: [Pokemon] -> Bool
-- PRECOND: Ninguna.
tieneAlMenosUnPokemonDeCada p = longitud (tiposDiferentesEn p) == 3

tiposDiferentesEn :: [Pokemon] -> [TipoDePokemon]
-- PRECOND: Ninguna.
tiposDiferentesEn []     = []
tiposDiferentesEn (p:ps) = if not (estaElTipoEn (tipoDePoke p) (tiposDiferentesEn ps))
                              then tipoDePoke p : tiposDiferentesEn ps
                              else tiposDiferentesEn ps

estaElTipoEn :: TipoDePokemon -> [TipoDePokemon] -> Bool
-- PRECOND: Ninguna.
estaElTipoEn tp []     = False
estaElTipoEn tp (t:ts) = esDelMismoTipo tp t || estaElTipoEn tp ts


-- EJERCICIO 3.3:

data Seniority = Junior | SemiSenior | Senior
    deriving Show

data Proyecto = ConsProyecto String
--                           Nombre del proyecto
{- INV. REP:
    - El nombre del proyecto no es vacío. 
-}
    deriving Show

data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
    deriving Show

data Empresa = ConsEmpresa [Rol]
    deriving Show


proyectos :: Empresa -> [Proyecto]
-- PRECOND: Ninguna.
proyectos (ConsEmpresa r) = proyectosEnLosQueTrabaja r

proyectosEnLosQueTrabaja :: [Rol] -> [Proyecto]
-- PRECOND: Ninguna.
proyectosEnLosQueTrabaja []     = []
proyectosEnLosQueTrabaja (r:rs) = if not (estaElProyectoDeEn (proyectoDelRol r) (proyectosEnLosQueTrabaja rs))
                                     then proyectoDelRol r : proyectosEnLosQueTrabaja rs
                                     else proyectosEnLosQueTrabaja rs

proyectoDelRol :: Rol -> Proyecto
-- PRECOND: Ninguna.
proyectoDelRol (Developer  _ p) = p
proyectoDelRol (Management _ p) = p

estaElProyectoDeEn :: Proyecto -> [Proyecto] -> Bool
-- PRECOND: Ninguna.
estaElProyectoDeEn po []     = False
estaElProyectoDeEn po (p:ps) = esElMismoProyecto po p || estaElProyectoDeEn po ps

esElMismoProyecto :: Proyecto -> Proyecto -> Bool
-- PRECOND: Ninguna.
esElMismoProyecto (ConsProyecto n1) (ConsProyecto n2) = n1 == n2



losDevSenior :: Empresa -> [Proyecto] -> Int
-- PRECOND: Ninguna.
losDevSenior (ConsEmpresa r) ps = longitud (seniorsDeQueTrabajanEn r ps)

seniorsDeQueTrabajanEn :: [Rol] -> [Proyecto] -> [Rol]
-- PRECOND: Ninguna.
seniorsDeQueTrabajanEn []     ps = []
seniorsDeQueTrabajanEn (r:rs) ps = if esSenior r && perteneceAAlgunProyectoDe r ps
                                      then r : seniorsDeQueTrabajanEn rs ps
                                      else seniorsDeQueTrabajanEn rs ps

esSenior :: Rol -> Bool
-- PRECOND: Ninguna.
esSenior (Developer  Senior _) = True
esSenior (Management Senior _) = True
esSenior _                     = False

perteneceAAlgunProyectoDe :: Rol -> [Proyecto] -> Bool
-- PRECOND: Ninguna.
perteneceAAlgunProyectoDe r []     = False
perteneceAAlgunProyectoDe r (p:ps) = esElMismoProyecto (proyectoDelRol r) p || perteneceAAlgunProyectoDe r ps



cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
-- PRECOND: Ninguna.
cantQueTrabajanEn ps (ConsEmpresa r) = longitud (empleadosDeQueTrabajanAlMenosEnAlgun r ps)

empleadosDeQueTrabajanAlMenosEnAlgun :: [Rol] -> [Proyecto] -> [Rol]
-- PRECOND: Ninguna.
empleadosDeQueTrabajanAlMenosEnAlgun []     _  = []
empleadosDeQueTrabajanAlMenosEnAlgun (r:rs) ps = if empleadoTrabajaEnAlgunProyectoDe r ps
                                                    then r : empleadosDeQueTrabajanAlMenosEnAlgun rs ps
                                                    else empleadosDeQueTrabajanAlMenosEnAlgun rs ps

empleadoTrabajaEnAlgunProyectoDe :: Rol -> [Proyecto] -> Bool
-- PRECOND: Ninguna.
empleadoTrabajaEnAlgunProyectoDe r []     = False
empleadoTrabajaEnAlgunProyectoDe r (p:ps) = esElMismoProyecto (proyectoDelRol r) p || empleadoTrabajaEnAlgunProyectoDe r ps



asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
-- PRECOND: Ninguna.
asignadosPorProyecto (ConsEmpresa xs) = paresDeCadaProyectoConSusIntegrantes xs []

paresDeCadaProyectoConSusIntegrantes :: [Rol] -> [(Proyecto, Int)] -> [(Proyecto, Int)]
-- PRECOND: Ninguna.
paresDeCadaProyectoConSusIntegrantes []     ys = ys
paresDeCadaProyectoConSusIntegrantes (x:xs) ys = paresDeCadaProyectoConSusIntegrantes xs (parDeProyectoConSusIntegrantes x ys)

parDeProyectoConSusIntegrantes :: Rol -> [(Proyecto, Int)] -> [(Proyecto, Int)]
-- PRECOND: Ninguna.
parDeProyectoConSusIntegrantes r []         = [(proyectoDelRol r, 1)]
parDeProyectoConSusIntegrantes r ((x, y):xs) = if esElMismoProyecto (proyectoDelRol r) x
                                                  then (x, y+1):xs
                                                  else (x, y) : parDeProyectoConSusIntegrantes r xs


-- CONSIDERACIÓN FINAL: Entiendo que hay muchas funciones que deberían tener una subtarea en el caso recursivo en lugar de un if, pero en muchos casos 
--                      eso afectaría a la legibilidad del código. Por eso, solamente modifiqué las funciones que eran viables a dicho cambio.