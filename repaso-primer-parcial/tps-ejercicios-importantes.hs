-- TRABAJO PRÁCTICO 4:

-- ##################################################### EJERCICIO 1 #####################################################

data Pizza = Prepizza | Capa Ingrediente Pizza
    deriving Show

data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int
    deriving Show

-- ################################################# FUNCIONES DE PRUEBA #################################################

pizza0 :: Pizza
-- Pizza de Jamón y Morrones con 8 aceitunas.
pizza0 = Capa (Aceitunas 8) (Capa Jamon (Capa Salsa (Capa Queso Prepizza)))

pizza1 :: Pizza
-- Pizza con solo Jamon.
pizza1 = Capa Jamon (Capa Jamon (Capa Jamon Prepizza))

pizza2 :: Pizza
-- Pizza con solo Salsa y Queso.
pizza2 = Capa Queso (Capa Salsa (Capa Queso Prepizza))

-- #######################################################################################################################

-- EJERCICIO 1.1:

cantidadDeCapas :: Pizza -> Int
-- PROPÓSITO: Dada una pizza devuelve la cantidad de ingredientes.
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p


-- EJERCICIO 1.2:

armarPizza :: [Ingrediente] -> Pizza
-- PROPÓSITO: Dada una lista de ingredientes construye una pizza.
armarPizza []     = Prepizza
armarPizza (i:is) = Capa i (armarPizza is)


-- EJERCICIO 1.3:

sacarJamon :: Pizza -> Pizza
-- PROPÓSITO: Le saca los ingredientes que sean jamón a la pizza.
sacarJamon Prepizza   = Prepizza
sacarJamon (Capa i p) = case i of
                          Jamon -> sacarJamon p
                          _     -> Capa i (sacarJamon p)


-- EJERCICIO 1.4:

tieneSoloSalsaYQueso :: Pizza -> Bool
-- PROPÓSITO: Dice si una pizza tiene solamente salsa y queso (o sea, no tiene de otros ingredientes. 
--            En particular, la prepizza, al no tener ningún ingrediente, debería dar verdadero.)
tieneSoloSalsaYQueso p = verSiTieneSalsaYQueso p False False

verSiTieneSalsaYQueso :: Pizza -> Bool -> Bool -> Bool
verSiTieneSalsaYQueso Prepizza   tieneSalsa tieneQueso = tieneSalsa && tieneQueso
verSiTieneSalsaYQueso (Capa i p) salsa      queso      = case i of
                                                           Salsa -> verSiTieneSalsaYQueso p True queso
                                                           Queso -> verSiTieneSalsaYQueso p salsa True
                                                           _     -> False


-- EJERCICIO 1.5:

duplicarAceitunas :: Pizza -> Pizza
-- PROPÓSITO: Recorre cada ingrediente y si es aceitunas duplica su cantidad.
duplicarAceitunas Prepizza   = Prepizza
duplicarAceitunas (Capa i p) = case i of
                                 Aceitunas n -> Capa (Aceitunas (n * 2)) (duplicarAceitunas p)
                                 _           -> Capa i (duplicarAceitunas p)


-- EJERCICIO 1.6:

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
-- PROPÓSITO: Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad 
--            de ingredientes de la pizza, y la respectiva pizza como segunda componente.
cantCapasPorPizza []     = []
cantCapasPorPizza (p:ps) = let cantCapasP = cantidadDeCapas p 
                            in (cantCapasP, p) : cantCapasPorPizza ps


-- ##################################################### EJERCICIO 2 #####################################################

data Dir = Izq | Der
    deriving Show

data Objeto = Tesoro | Chatarra
    deriving Show

data Cofre = Cofre [Objeto]
    deriving Show

data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa
    deriving Show

-- ################################################# FUNCIONES DE PRUEBA #################################################

mapa0 :: Mapa
-- Mapa con tres bifurcaciones, con seis cofres y en uno de ellos un tesoro.
mapa0 = Bifurcacion (Cofre [Chatarra, Chatarra]) 
            (Bifurcacion (Cofre [Chatarra, Tesoro])
                (Fin (Cofre [Chatarra]))
                (Fin (Cofre []))
            ) 
            (Bifurcacion (Cofre [Chatarra, Chatarra])
                (Bifurcacion (Cofre [Chatarra])
                    (Fin (Cofre [Chatarra]))
                    (Fin (Cofre []))
                )
                (Fin (Cofre []))
            )
{-
       C
     /   \
   CT     C
   / \   / \
  C   C C   C
           / \
          C   C
-}

-- #######################################################################################################################

-- EJERCICIO 2.1:

hayTesoro :: Mapa -> Bool
-- PROPÓSITO: Indica si hay un tesoro en alguna parte del mapa.
hayTesoro (Fin c)               = hayTesoroEnC c
hayTesoro (Bifurcacion c m1 m2) = hayTesoroEnC c || hayTesoro m1 || hayTesoro m2

hayTesoroEnC :: Cofre -> Bool
hayTesoroEnC (Cofre o) = hayTesoroEnO o

hayTesoroEnO :: [Objeto] -> Bool
hayTesoroEnO []     = False
hayTesoroEnO (o:os) = esTesoro o || hayTesoroEnO os

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False


-- EJERCICIO 2.2:

hayTesoroEn :: [Dir] -> Mapa -> Bool
-- PROPÓSITO: Indica si al final del camino hay un tesoro. Nota: el final de un 
--            camino se representa con una lista vacía de direcciones.
hayTesoroEn []     (Fin c)               = hayTesoroEnC c
hayTesoroEn []     (Bifurcacion c m1 m2) = hayTesoroEnC c
hayTesoroEn _      (Fin c)               = False
hayTesoroEn (d:ds) (Bifurcacion c m1 m2) = case d of
                                             Izq -> hayTesoroEn ds m1
                                             Der -> hayTesoroEn ds m2


-- EJERCICIO 2.3:

caminoAlTesoro :: Mapa -> [Dir]
-- PROPÓSITO: Indica el camino al tesoro. 
-- PRECONDICIÓN: Existe un tesoro y es único.
caminoAlTesoro (Fin c)               = []
caminoAlTesoro (Bifurcacion c m1 m2) = if hayTesoroEnC c
                                          then []
                                          else if hayTesoro m1 
                                                  then Izq : caminoAlTesoro m1
                                                  else Der : caminoAlTesoro m2


-- EJERCICIO 2.4:

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
-- PROPÓSITO: Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga (Fin c)               = []
caminoDeLaRamaMasLarga (Bifurcacion c m1 m2) = if length (caminoDeLaRamaMasLarga m1) > length (caminoDeLaRamaMasLarga m2)
                                                  then Izq : caminoDeLaRamaMasLarga m1
                                                  else Der : caminoDeLaRamaMasLarga m2


-- EJERCICIO 2.5:

tesorosPorNivel :: Mapa -> [[Objeto]]
-- PROPÓSITO: Devuelve los tesoros separados por nivel en el árbol.
tesorosPorNivel (Fin c)               = [tesorosDeC c]
tesorosPorNivel (Bifurcacion c m1 m2) = [tesorosDeC c] ++ unirNiveles (tesorosPorNivel m1)  (tesorosPorNivel m2)

tesorosDeC :: Cofre -> [Objeto]
tesorosDeC (Cofre o) = tesorosDeO o

tesorosDeO :: [Objeto] -> [Objeto]
tesorosDeO []     = []
tesorosDeO (o:os) = if esTesoro o
                       then o : tesorosDeO os
                       else tesorosDeO os

unirNiveles :: [[Objeto]] -> [[Objeto]] -> [[Objeto]]
unirNiveles []     ys     = ys
unirNiveles xs     []     = xs
unirNiveles (x:xs) (y:ys) = (x ++ y) : unirNiveles xs ys 


-- EJERCICIO 2.6:

todosLosCaminos :: Mapa -> [[Dir]]
-- PROPÓSITO: Devuelve todos lo caminos en el mapa
todosLosCaminos (Fin c)               = []
todosLosCaminos (Bifurcacion c m1 m2) = [Izq] : consACada Izq (todosLosCaminos m1) ++
                                        [Der] : consACada Der (todosLosCaminos m2)

consACada :: Dir -> [[Dir]] -> [[Dir]]
consACada d []       = []
consACada d (ds:dss) = (d:ds) : consACada d dss


-- ##################################################### EJERCICIO 3 #####################################################

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

-- ################################################# FUNCIONES DE PRUEBA #################################################

nave0 :: Nave
nave0 = N (NodeT (S "NNO118" [LanzaTorpedos, LanzaTorpedos, (Motor 90)] ["Jorge", "Ricardo", "Micaela"]) 
            (EmptyT) 
            (NodeT (S "NRG540" [(Motor 10), (Almacen [Combustible])] ["Marcelo", "Analia"])  
                (EmptyT)
                (NodeT (S "VYW777" [(Almacen [Comida, Oxigeno, Torpedo]), (Almacen [Combustible])] ["Ricardo"])  
                    (EmptyT)
                    (EmptyT)
                )
            )
          )

-- #######################################################################################################################

-- EJERCICIO 3.1:

sectores :: Nave -> [SectorId]
-- PROPÓSITO: Devuelve todos los sectores de la nave.
sectores (N ts) = sectoresDeTS ts

sectoresDeTS :: Tree Sector -> [SectorId]
sectoresDeTS EmptyT            = []
sectoresDeTS (NodeT s ts1 ts2) = sectorDeS s : (sectoresDeTS ts1 ++ sectoresDeTS ts2)

sectorDeS :: Sector -> SectorId
sectorDeS (S sid _ _) = sid 


-- EJERCICIO 3.2:

poderDePropulsion :: Nave -> Int
-- PROPÓSITO: Devuelve la suma de poder de propulsión de todos los motores de la nave. 
-- NOTA: El poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion (N ts) = poderDePropulsionDeTS ts

poderDePropulsionDeTS :: Tree Sector -> Int
poderDePropulsionDeTS EmptyT            = 0
poderDePropulsionDeTS (NodeT s ts1 ts2) = poderDePropulsionDeS s + poderDePropulsionDeTS ts1 + poderDePropulsionDeTS ts2

poderDePropulsionDeS :: Sector -> Int
poderDePropulsionDeS (S _ cs _) = poderDePropulsionDeCS cs

poderDePropulsionDeCS :: [Componente] -> Int
poderDePropulsionDeCS []     = 0
poderDePropulsionDeCS (c:cs) = case c of
                                 Motor n -> n + poderDePropulsionDeCS cs
                                 _       -> poderDePropulsionDeCS cs


-- EJERCICIO 3.3:

barriles :: Nave -> [Barril]
-- PROPÓSITO: Devuelve todos los barriles de la nave.
barriles (N ts) = barrilesDeTS ts

barrilesDeTS :: Tree Sector -> [Barril]
barrilesDeTS EmptyT            = []
barrilesDeTS (NodeT s ts1 ts2) = barrilesDeS s ++ barrilesDeTS ts1 ++ barrilesDeTS ts2

barrilesDeS :: Sector -> [Barril]
barrilesDeS (S _ cs _) = barrilesDeCS cs

barrilesDeCS :: [Componente] -> [Barril]
barrilesDeCS []     = []
barrilesDeCS (c:cs) = case c of
                        Almacen b -> b ++ barrilesDeCS cs
                        _         -> barrilesDeCS cs


-- EJERCICIO 3.4:

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
-- PROPÓSITO: Añade una lista de componentes a un sector de la nave.
-- NOTA: Ese sector puede no existir, en cuyo caso no añade componentes.
agregarASector cs sid (N ts) = N (agregarASectorDeTS cs sid ts)

agregarASectorDeTS :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarASectorDeTS cs sid EmptyT            = EmptyT
agregarASectorDeTS cs sid (NodeT s ts1 ts2) = let nuevoS = agregarASectorDeS cs s;
                                               in if sectorDeS s == sid
                                                     then NodeT nuevoS ts1 ts2
                                                     else NodeT s (agregarASectorDeTS cs sid ts1)
                                                                  (agregarASectorDeTS cs sid ts2)

agregarASectorDeS :: [Componente] -> Sector -> Sector
agregarASectorDeS cs (S sid cs1 tr) = S sid (cs1 ++ cs) tr 


-- EJERCICIO 3.5:

asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
-- PROPÓSITO: Incorpora un tripulante a una lista de sectores de la nave.
-- PRECONDICIÓN: Todos los id de la lista existen en la nave.
asignarTripulanteA t sids (N ts) = N (asignarTripulanteTS t sids ts)

asignarTripulanteTS :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteTS t _    EmptyT            = EmptyT
asignarTripulanteTS t []   (NodeT s ts1 ts2) = NodeT s ts1 ts2
asignarTripulanteTS t sids (NodeT s ts1 ts2) = let nuevoS = asignarTripulanteS t s;
                                                   nuevoSids = borrarSectorEnSids (sectorDeS s) sids;
                                                in if elem (sectorDeS s) sids
                                                      then NodeT nuevoS (asignarTripulanteTS t nuevoSids ts1) 
                                                                        (asignarTripulanteTS t nuevoSids ts2)
                                                      else NodeT s (asignarTripulanteTS t sids ts1) 
                                                                   (asignarTripulanteTS t sids ts2)

asignarTripulanteS :: Tripulante -> Sector -> Sector
asignarTripulanteS t (S sid cs tr) = S sid cs (t:tr)

borrarSectorEnSids :: SectorId -> [SectorId] -> [SectorId]
borrarSectorEnSids sd []         = []
borrarSectorEnSids sd (sid:sids) = if sd == sid
                                      then sids
                                      else sid : borrarSectorEnSids sd sids


-- EJERCICIO 3.6:

sectoresAsignados :: Tripulante -> Nave -> [SectorId]
-- PROPÓSITO: Devuelve los sectores en donde aparece un tripulante dado.
sectoresAsignados t (N ts) = sectoresAsignadosEnTS t ts

sectoresAsignadosEnTS :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignadosEnTS t EmptyT            = [] 
sectoresAsignadosEnTS t (NodeT s ts1 ts2) = if estaAsignadoEnS t s
                                               then (sectorDeS s) : (sectoresAsignadosEnTS t ts1) ++ 
                                                                    (sectoresAsignadosEnTS t ts2)  
                                               else (sectoresAsignadosEnTS t ts1) ++ (sectoresAsignadosEnTS t ts2)  

estaAsignadoEnS :: Tripulante -> Sector -> Bool
estaAsignadoEnS t (S _ _ tr) = elem t tr


-- EJERCICIO 3.7:

tripulantes :: Nave -> [Tripulante]
-- PROPÓSITO: Devuelve la lista de tripulantes, sin elementos repetidos.
tripulantes (N ts) = tripulantesDeTS ts

tripulantesDeTS :: Tree Sector -> [Tripulante]
tripulantesDeTS EmptyT            = []
tripulantesDeTS (NodeT s ts1 ts2) = sumarTripulantesA (tripulantesDeS s) (tripulantesDeTS ts1 ++ tripulantesDeTS ts2)

sumarTripulantesA :: [Tripulante] -> [Tripulante] -> [Tripulante]
sumarTripulantesA []       ts2 = ts2
sumarTripulantesA ts1      []  = ts1
sumarTripulantesA (t1:ts1) ts2 = if elem t1 ts2
                                    then sumarTripulantesA ts1 ts2
                                    else t1 : sumarTripulantesA ts1 ts2

tripulantesDeS :: Sector -> [Tripulante]
tripulantesDeS (S _ _ tr) = tr