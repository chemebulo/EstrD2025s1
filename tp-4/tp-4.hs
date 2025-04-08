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



-------------------------------------------------------------------------------------------------------

-- EJERCICIO 1.1

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas = undefined


-- EJERCICIO 1.2

armarPizza :: [Ingrediente] -> Pizza
armarPizza = undefined


-- EJERCICIO 1.3

sacarJamon :: Pizza -> Pizza
sacarJamon = undefined


-- EJERCICIO 1.4

tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso = undefined


-- EJERCICIO 1.5

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas = undefined


-- EJERCICIO 1.6

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza = undefined


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
hayTesoro = undefined


-- EJERCICIO 2.2

hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn = undefined


-- EJERCICIO 2.3

caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro = undefined


-- EJERCICIO 2.4

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga = undefined


-- EJERCICIO 2.5

tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel = undefined


-- EJERCICIO 2.6

todosLosCaminos :: Mapa -> [[Dir]]
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
sectores = undefined


-- EJERCICIO 3.2

poderDePropulsion :: Nave -> Int
poderDePropulsion = undefined


-- EJERCICIO 3.3

barriles :: Nave -> [Barril]
barriles = undefined


-- EJERCICIO 3.4

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector = undefined


-- EJERCICIO 3.5

asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA = undefined


-- EJERCICIO 3.6

sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados = undefined


-- EJERCICIO 3.7

tripulantes :: Nave -> [Tripulante]
tripulantes = undefined 


-- PUNTO 4 (Manada de Lobos):

type Presa      = String -- nombre de presa

type Territorio = String -- nombre de territorio

type Nombre     = String -- nombre de lobo

data Lobo       = Cazador    Nombre [Presa]      Lobo Lobo Lobo
                | Explorador Nombre [Territorio] Lobo Lobo
                | Cría       Nombre
    deriving Show

data Manada = M Lobo
    deriving Show

----------------------------------------- FUNCIONES DE PRUEBA -----------------------------------------



-------------------------------------------------------------------------------------------------------

-- EJERCICIO 4.1




-- EJERCICIO 4.2

buenaCaza :: Manada -> Bool
buenaCaza = undefined


-- EJERCICIO 4.3

elAlfa :: Manada -> (Nombre, Int)
elAlfa = undefined


-- EJERCICIO 4.4

losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron = undefined


-- EJERCICIO 4.5

exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio = undefined


-- EJERCICIO 4.6

cazadoresSuperioresDe :: Nombre -> Manada -> [Nombre]
cazadoresSuperioresDe = undefined