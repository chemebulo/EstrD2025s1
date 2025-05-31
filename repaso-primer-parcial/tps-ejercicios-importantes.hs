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
asignarTripulanteS t (S sid cs tr) = if elem t tr
                                        then S sid cs tr
                                        else S sid cs (t:tr)

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


-- ##################################################### EJERCICIO 4 #####################################################

type Presa = String -- Nombre de presa.

type Territorio = String -- Nombre de territorio.

type Nombre = String -- Nombre de lobo.

data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre
    deriving Show

data Manada = M Lobo
    deriving Show

-- ################################################# FUNCIONES DE PRUEBA #################################################

manada0 :: Manada
manada0 = M (lobo0)

lobo0 :: Lobo 
lobo0 = Cazador "DienteFiloso" ["Bufalos", "Antilopes"]
                    (Cria "Hopito")
                    (Explorador "Incansable" ["Oeste hasta el rio", "Bosque este"]
                        (Cria "MechonGris")
                        (Cria "Rabito")
                    )
                    (Cazador "Garras" ["Antilopes", "Ciervos"]
                        (Explorador "Zarpado" ["Bosque este"]
                            (Cria "Osado")
                            (Cazador "Mandibulas" ["Cerdos", "Pavos", "Vacas", "Ciervos"]
                                (Cria "Desgreñado")
                                (Cria "Malcriado")
                                (Cazador "TrituraHuesos" ["Conejos", "Conejos", "Vacas"]
                                    (Cria "Peludo")
                                    (Cria "Largo")
                                    (Cria "Menudo")
                                )
                            )
                        )
                        (Cria "Garrita")
                        (Cria "Manchas")
                    )

-- #######################################################################################################################

-- EJERCICIO 4.1:

{- Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean crías. 
   Resolver las siguientes funciones utilizando recursión estructural sobre la estructura que corresponda en cada caso:-}


-- EJERCICIO 4.2:

buenaCaza :: Manada -> Bool
-- PROPÓSITO: Dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad de crías.
buenaCaza (M l) = cantidadDeAlimentoEnL l > cantidadDeCriasEnL l

cantidadDeAlimentoEnL :: Lobo -> Int 
cantidadDeAlimentoEnL (Cria n)                = 0
cantidadDeAlimentoEnL (Explorador n ts l1 l2) = cantidadDeAlimentoEnL l1 + cantidadDeAlimentoEnL l2
cantidadDeAlimentoEnL (Cazador n ps l1 l2 l3) = let cantidadDeAlimentoEnPs = length ps
                                                 in cantidadDeAlimentoEnPs   + cantidadDeAlimentoEnL l1 +
                                                    cantidadDeAlimentoEnL l2 + cantidadDeAlimentoEnL l3

cantidadDeCriasEnL :: Lobo -> Int 
cantidadDeCriasEnL (Cria n)                = 1
cantidadDeCriasEnL (Explorador n ts l1 l2) = cantidadDeCriasEnL l1 + cantidadDeCriasEnL l2
cantidadDeCriasEnL (Cazador n ps l1 l2 l3) = cantidadDeCriasEnL l1 + cantidadDeCriasEnL l2 + cantidadDeCriasEnL l3


-- EJERCICIO 4.3:

elAlfa :: Manada -> (Nombre, Int)
-- PROPÓSITO: Dada una manada, devuelve el nombre del lobo con más presas cazadas, junto con su cantidad de presas. 
-- NOTA: Se considera que los exploradores y crías tienen cero presas cazadas, y que podrían formar parte del resultado
--       si es que no existen cazadores con más de cero presas.
elAlfa (M l) = elAlfaEnL l

elAlfaEnL :: Lobo -> (Nombre, Int)
elAlfaEnL (Cria n)                = (n, 0)
elAlfaEnL (Explorador n ts l1 l2) = elAlfaEntre (n, 0) (elAlfaEntre  (elAlfaEnL l1) (elAlfaEnL l2))
elAlfaEnL (Cazador n ps l1 l2 l3) = elAlfaEntre (elAlfaEntre (n, length ps) (elAlfaEnL l1))
                                                (elAlfaEntre (elAlfaEnL l2) (elAlfaEnL l3))

elAlfaEntre :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int) 
elAlfaEntre (n1, ps1) (n2, ps2) = if ps1 > ps2
                                     then (n1, ps1)
                                     else (n2, ps2)


-- EJERCICIO 4.4:

losQueExploraron :: Territorio -> Manada -> [Nombre]
-- PROPÓSITO: Dado un territorio y una manada, devuelve los nombres de los exploradores que pasaron por dicho territorio.
losQueExploraron t (M l) = losQueExploraronTEnL t l

losQueExploraronTEnL :: Territorio -> Lobo -> [Nombre]
losQueExploraronTEnL t (Cria n)                = []
losQueExploraronTEnL t (Explorador n ts l1 l2) = if elem t ts
                                                    then n : losQueExploraronTEnL t l1 ++ losQueExploraronTEnL t l2
                                                    else losQueExploraronTEnL t l1 ++ losQueExploraronTEnL t l2
losQueExploraronTEnL t (Cazador n ps l1 l2 l3) = losQueExploraronTEnL t l1 ++ losQueExploraronTEnL t l2 ++ 
                                                 losQueExploraronTEnL t l3


-- EJERCICIO 4.5:

exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
-- PROPÓSITO: Dada una manada, denota la lista de los pares cuyo primer elemento es un territorio y cuyo segundo elemento es la
--            lista de los nombres de los exploradores que exploraron dicho territorio. Los territorios no deben repetirse.
exploradoresPorTerritorio (M l) = exploradoresPorTerritorioEnL l

exploradoresPorTerritorioEnL :: Lobo -> [(Territorio, [Nombre])]
exploradoresPorTerritorioEnL (Cria n)                = []
exploradoresPorTerritorioEnL (Explorador n ts l1 l2) = agregarExploradorATS n ts (integrarExploradores
                                                                                    (exploradoresPorTerritorioEnL l1)
                                                                                    (exploradoresPorTerritorioEnL l2)
                                                                                 )
exploradoresPorTerritorioEnL (Cazador n ps l1 l2 l3) = integrarExploradores (exploradoresPorTerritorioEnL l1) 
                                                                            (integrarExploradores 
                                                                                (exploradoresPorTerritorioEnL l2) 
                                                                                (exploradoresPorTerritorioEnL l3)
                                                                            )

integrarExploradores :: [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
integrarExploradores []             tns2 = tns2
integrarExploradores tns1           []   = tns1
integrarExploradores ((t, ns):tns1) tns2 = if territorioEstaEn t tns2
                                              then integrarExploradores tns1 (agregarExploradores ns t tns2)
                                              else (t, ns) : integrarExploradores tns1 tns2 
                                               
territorioEstaEn :: Territorio -> [(Territorio, [Nombre])] -> Bool
territorioEstaEn t1 []            = False
territorioEstaEn t1 ((t2, _):tns) = t1 == t2 || territorioEstaEn t1 tns

agregarExploradores :: [Nombre] -> Territorio -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] 
agregarExploradores []     t tns = tns
agregarExploradores (n:ns) t tns = agregarExploradores ns t (agregarExploradorAT n t tns)

agregarExploradorATS :: Nombre -> [Territorio] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
agregarExploradorATS n []     tns = tns
agregarExploradorATS n (t:ts) tns = agregarExploradorATS n ts (agregarExploradorAT n t tns)

agregarExploradorAT :: Nombre -> Territorio -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
agregarExploradorAT n1 t1 []               = [(t1, [n1])]
agregarExploradorAT n1 t1 ((t2, ns2):tns2) = if t1 == t2 
                                               then (t1, n1:ns2) : tns2
                                               else (t2, ns2) : (agregarExploradorAT n1 t1 tns2)


-- EJERCICIO 4.6:

cazadoresSuperioresDe :: Nombre -> Manada -> [Nombre]
-- PROPÓSITO: Dado el nombre de un lobo y una manada, indica el nombre de todos los cazadores que tienen como subordinado
--            al lobo dado (puede ser un subordinado directo, o el subordinado de un subordinado).
-- PRECONDICIÓN: Hay un lobo con dicho nombre y es único.
cazadoresSuperioresDe n (M l) = cazadoresSuperioresDeL n l

cazadoresSuperioresDeL :: Nombre -> Lobo -> [Nombre]
cazadoresSuperioresDeL nb (Cria n)                = []
cazadoresSuperioresDeL nb (Explorador n ts l1 l2) = if nb == n
                                                       then []
                                                       else if estaLoboEn nb l1
                                                               then cazadoresSuperioresDeL nb l1
                                                               else cazadoresSuperioresDeL nb l2
cazadoresSuperioresDeL nb (Cazador n ps l1 l2 l3) = if nb == n
                                                       then []
                                                       else if estaLoboEn nb l1 
                                                               then n : cazadoresSuperioresDeL nb l1
                                                               else if estaLoboEn nb l2
                                                                       then n : cazadoresSuperioresDeL nb l2
                                                                       else n : cazadoresSuperioresDeL nb l3 

estaLoboEn :: Nombre -> Lobo -> Bool
estaLoboEn nb (Cria n)                = (nb == n)
estaLoboEn nb (Explorador n ts l1 l2) = (nb == n) || estaLoboEn nb l1 || estaLoboEn nb l2 
estaLoboEn nb (Cazador n ps l1 l2 l3) = (nb == n) || estaLoboEn nb l1 || estaLoboEn nb l2 || estaLoboEn nb l3


-- TRABAJO PRÁCTICO 5:

-- ##################################################### EJERCICIO 2 #####################################################

-- EJERCICIO 2.1:


-- EJERCICIO 2.2:


-- EJERCICIO 2.3: