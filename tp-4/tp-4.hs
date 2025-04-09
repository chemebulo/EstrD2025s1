-- PUNTO 1 (Pizzas):

data Pizza = Prepizza
           | Capa Ingrediente Pizza
    deriving Show

data Ingrediente = Salsa
                 | Queso
                 | Jamon
                 | Aceitunas Int
    deriving Show

----------------------------------------- FUNCIONES DE PRUEBA -----------------------------------------

pizza0 :: Pizza
pizza0 = Prepizza

pizza1 :: Pizza
pizza1 = Capa (Aceitunas 5) (Capa Queso (Capa Salsa Prepizza))

pizza2 :: Pizza
pizza2 =  Capa (Aceitunas 10) (Capa Jamon (Capa Queso (Capa Salsa Prepizza)))

-------------------------------------------------------------------------------------------------------

-- EJERCICIO 1.1

cantidadDeCapas :: Pizza -> Int
-- PRECOND: Ninguna.
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p


-- EJERCICIO 1.2

armarPizza :: [Ingrediente] -> Pizza
-- PRECOND: Ninguna.
armarPizza []     = Prepizza
armarPizza (i:is) = Capa i (armarPizza is)


-- EJERCICIO 1.3

sacarJamon :: Pizza -> Pizza
-- PRECOND: Ninguna.
sacarJamon Prepizza   = Prepizza
sacarJamon (Capa i p) = if esJamon i
                           then sacarJamon p
                           else Capa i (sacarJamon p)

esJamon :: Ingrediente -> Bool
-- PRECOND: Ninguna.
esJamon Jamon = True
esJamon _     = False


-- EJERCICIO 1.4

tieneSoloSalsaYQueso :: Pizza -> Bool
-- PRECOND: Ninguna.
tieneSoloSalsaYQueso Prepizza   = True
tieneSoloSalsaYQueso (Capa i p) = (esIngredienteIgualQue Salsa i && esSoloCapaDeEn Queso p) || (esIngredienteIgualQue Queso i && esSoloCapaDeEn Salsa p)

esIngredienteIgualQue :: Ingrediente -> Ingrediente -> Bool
-- PRECOND: Ninguna.
esIngredienteIgualQue Queso Queso = True
esIngredienteIgualQue Salsa Salsa = True
esIngredienteIgualQue Jamon Jamon = True
esIngredienteIgualQue _     _     = False

esSoloCapaDeEn :: Ingrediente -> Pizza -> Bool
-- PRECOND: Ninguna.
esSoloCapaDeEn ing Prepizza   = False
esSoloCapaDeEn ing (Capa i p) = esIngredienteIgualQue ing i && esPrepizza p

esPrepizza :: Pizza -> Bool
-- PRECOND: Ninguna.
esPrepizza Prepizza = True
esPrepizza _        = False


-- EJERCICIO 1.5

duplicarAceitunas :: Pizza -> Pizza
-- PRECOND: Ninguna.
duplicarAceitunas Prepizza   = Prepizza
duplicarAceitunas (Capa i p) = Capa (duplicarSiSonAceitunas i) (duplicarAceitunas p)

duplicarSiSonAceitunas :: Ingrediente -> Ingrediente
-- PRECOND: Ninguna.
duplicarSiSonAceitunas (Aceitunas n) = Aceitunas (n*2)
duplicarSiSonAceitunas i             = i


-- EJERCICIO 1.6

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
-- PRECOND: Ninguna.
cantCapasPorPizza []     = []
cantCapasPorPizza (p:ps) = (cantidadDeCapas p, p) : cantCapasPorPizza ps


-- PUNTO 2 (Mapa de Tesoros (con bifucaciones)):

data Dir = Izq | Der
    deriving Show

data Objeto = Tesoro | Chatarra
    deriving Show

data Cofre = Cofre [Objeto]
    deriving Show

data Mapa = Fin Cofre
          | Bifurcacion Cofre Mapa Mapa
    deriving Show

----------------------------------------- FUNCIONES DE PRUEBA -----------------------------------------



-------------------------------------------------------------------------------------------------------

-- EJERCICIO 2.1

hayTesoro :: Mapa -> Bool
-- PRECOND: Ninguna.
hayTesoro = undefined


-- EJERCICIO 2.2

hayTesoroEn :: [Dir] -> Mapa -> Bool
-- PRECOND: Ninguna.
hayTesoroEn = undefined


-- EJERCICIO 2.3

caminoAlTesoro :: Mapa -> [Dir]
-- PRECOND: Ninguna.
caminoAlTesoro = undefined


-- EJERCICIO 2.4

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
-- PRECOND: Ninguna.
caminoDeLaRamaMasLarga = undefined


-- EJERCICIO 2.5

tesorosPorNivel :: Mapa -> [[Objeto]]
-- PRECOND: Ninguna.
tesorosPorNivel = undefined


-- EJERCICIO 2.6

todosLosCaminos :: Mapa -> [[Dir]]
-- PRECOND: Ninguna.
todosLosCaminos = undefined


-- PUNTO 3 (Nave Espacial):

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
    deriving Show

data Barril = Comida | Oxigeno | Torpedo | Combustible
    deriving Show

data Sector = S SectorId [Componente] [Tripulante]
    deriving Show

type SectorId = String

type Tripulante = String

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

data Nave = N (Tree Sector)
    deriving Show

----------------------------------------- FUNCIONES DE PRUEBA -----------------------------------------



-------------------------------------------------------------------------------------------------------

-- EJERCICIO 3.1

sectores :: Nave -> [SectorId]
-- PRECOND: Ninguna.
sectores = undefined


-- EJERCICIO 3.2

poderDePropulsion :: Nave -> Int
-- PRECOND: Ninguna.
poderDePropulsion = undefined


-- EJERCICIO 3.3

barriles :: Nave -> [Barril]
-- PRECOND: Ninguna.
barriles = undefined


-- EJERCICIO 3.4

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
-- PRECOND: Ninguna.
agregarASector = undefined


-- EJERCICIO 3.5

asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
-- PRECOND: Ninguna.
asignarTripulanteA = undefined


-- EJERCICIO 3.6

sectoresAsignados :: Tripulante -> Nave -> [SectorId]
-- PRECOND: Ninguna.
sectoresAsignados = undefined


-- EJERCICIO 3.7

tripulantes :: Nave -> [Tripulante]
-- PRECOND: Ninguna.
tripulantes = undefined


-- PUNTO 4 (Manada de Lobos):

type Presa      = String -- nombre de presa

type Territorio = String -- nombre de territorio

type Nombre     = String -- nombre de lobo

data Lobo       = Cazador    Nombre [Presa]      Lobo Lobo Lobo
                | Explorador Nombre [Territorio] Lobo Lobo
                | Cria       Nombre
    deriving Show

data Manada = M Lobo
    deriving Show

----------------------------------------- FUNCIONES DE PRUEBA -----------------------------------------



-------------------------------------------------------------------------------------------------------

-- EJERCICIO 4.1




-- EJERCICIO 4.2

buenaCaza :: Manada -> Bool
-- PRECOND: Ninguna.
buenaCaza = undefined


-- EJERCICIO 4.3

elAlfa :: Manada -> (Nombre, Int)
-- PRECOND: Ninguna.
elAlfa = undefined


-- EJERCICIO 4.4

losQueExploraron :: Territorio -> Manada -> [Nombre]
-- PRECOND: Ninguna.
losQueExploraron = undefined


-- EJERCICIO 4.5

exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
-- PRECOND: Ninguna.
exploradoresPorTerritorio = undefined


-- EJERCICIO 4.6

cazadoresSuperioresDe :: Nombre -> Manada -> [Nombre]
-- PRECOND: Ninguna.
cazadoresSuperioresDe = undefined