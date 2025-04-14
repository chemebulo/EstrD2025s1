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
pizza2 = Capa (Aceitunas 10) (Capa Jamon (Capa Queso (Capa Salsa Prepizza)))

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

mapa0 :: Mapa
mapa0 = Fin (Cofre [Tesoro, Chatarra, Chatarra, Tesoro, Tesoro])

mapa1 :: Mapa
mapa1 = Bifurcacion (Cofre [Chatarra, Chatarra, Chatarra]) mapa2 mapa0

mapa2 :: Mapa
mapa2 = Bifurcacion (Cofre [Chatarra, Tesoro]) mapa0 mapa0

-- Gráfico: https://prnt.sc/wf7KOnJ8AGh0

-------------------------------------------------------------------------------------------------------

-- EJERCICIO 2.1

hayTesoro :: Mapa -> Bool
-- PRECOND: Ninguna.
hayTesoro (Fin c)               = hayTesoroEnCofre c
hayTesoro (Bifurcacion c m1 m2) = hayTesoroEnCofre c || hayTesoro m1 || hayTesoro m2

hayTesoroEnCofre :: Cofre -> Bool
-- PRECOND: Ninguna.
hayTesoroEnCofre (Cofre o) = hayTesoroEnObjetos o

hayTesoroEnObjetos :: [Objeto] -> Bool
-- PRECOND: Ninguna.
hayTesoroEnObjetos []     = False
hayTesoroEnObjetos (o:os) = esTesoro o || hayTesoroEnObjetos os

esTesoro :: Objeto -> Bool
-- PRECOND: Ninguna.
esTesoro Tesoro = True
esTesoro _      = False


-- EJERCICIO 2.2

hayTesoroEn :: [Dir] -> Mapa -> Bool
-- PRECOND: Ninguna.
hayTesoroEn []     m                     = hayTesoroEnCofreDeMapa m
hayTesoroEn (d:ds) (Fin c)               = False
hayTesoroEn (d:ds) (Bifurcacion c m1 m2) = if esIzquierda d
                                              then hayTesoroEn ds m1
                                              else hayTesoroEn ds m2

hayTesoroEnCofreDeMapa :: Mapa -> Bool
-- PRECOND: Ninguna.
hayTesoroEnCofreDeMapa (Fin c)             = hayTesoroEnCofre c
hayTesoroEnCofreDeMapa (Bifurcacion c _ _) = hayTesoroEnCofre c

esIzquierda :: Dir -> Bool
-- PRECOND: Ninguna.
esIzquierda Izq = True
esIzquierda Der = False


-- EJERCICIO 2.3

caminoAlTesoro :: Mapa -> [Dir]
-- PRECOND: Existe un tesoro y es único.
caminoAlTesoro (Fin c)               = []
caminoAlTesoro (Bifurcacion c m1 m2) = if hayTesoroEnCofre c
                                          then []
                                          else direccionAlMapaConTesoro m1 m2 : caminoAlTesoro (caminoConTesoroEntre m1 m2)

direccionAlMapaConTesoro :: Mapa -> Mapa -> Dir
-- PRECOND: Ninguna.
direccionAlMapaConTesoro m1 m2 = if hayTesoro m1
                                    then Izq
                                    else Der

caminoConTesoroEntre :: Mapa -> Mapa -> Mapa
-- PRECOND: Ninguna.
caminoConTesoroEntre m1 m2 = if hayTesoro m1
                                then m1
                                else m2


-- EJERCICIO 2.4

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
-- PRECOND: Ninguna.
caminoDeLaRamaMasLarga (Fin c)               = []
caminoDeLaRamaMasLarga (Bifurcacion c m1 m2) = if length (caminoDeLaRamaMasLarga m1) > length (caminoDeLaRamaMasLarga m2)
                                                  then Izq : caminoDeLaRamaMasLarga m1
                                                  else Der : caminoDeLaRamaMasLarga m2


-- EJERCICIO 2.5

tesorosPorNivel :: Mapa -> [[Objeto]]
-- PRECOND: Ninguna.
tesorosPorNivel (Fin c)               = [tesorosDeCofre c]
tesorosPorNivel (Bifurcacion c m1 m2) = tesorosDeCofre c : unirNivelesDe (tesorosPorNivel m1) (tesorosPorNivel m2)

tesorosDeCofre :: Cofre -> [Objeto]
-- PRECOND: Ninguna.
tesorosDeCofre (Cofre o) = tesorosDeObjetos o

tesorosDeObjetos :: [Objeto] -> [Objeto]
-- PRECOND: Ninguna.
tesorosDeObjetos []     = []
tesorosDeObjetos (o:os) = singularSi o (esTesoro o) ++ tesorosDeObjetos os

singularSi :: a -> Bool -> [a]
-- PRECOND: Ninguna.
singularSi x True  = [x]
singularSi x False = []

unirNivelesDe :: [[a]] -> [[a]] -> [[a]]
-- PRECOND: Ninguna.
unirNivelesDe []        ys       = ys
unirNivelesDe xs        []       = xs
unirNivelesDe (xs:xss)  (ys:yss) = (xs ++ ys) : unirNivelesDe xss yss


-- EJERCICIO 2.6

todosLosCaminos :: Mapa -> [[Dir]]
-- PRECOND: Ninguna.
todosLosCaminos (Fin c)               = []
todosLosCaminos (Bifurcacion _ m1 m2) = [Izq] : consACadaDe Izq (todosLosCaminos m1) ++
                                        [Der] : consACadaDe Der (todosLosCaminos m2)

consACadaDe :: a -> [[a]] -> [[a]]
-- PRECOND: Ninguna.
consACadaDe x []       = []
consACadaDe x (ys:yss) = (x:ys) : consACadaDe x yss


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

-------------------------------------------------------- FUNCIONES DE PRUEBA ---------------------------------------------------

nave1 :: Nave
nave1 = N (NodeT (S "a920" [LanzaTorpedos, (Almacen [Torpedo, Oxigeno])] ["jose", "manuel", "javier"])
              (NodeT (S "34b0" [(Almacen [Torpedo, Comida, Comida]), (Motor 98)] ["javier", "ricardo"]) EmptyT EmptyT)
              (NodeT (S "890ab" [(Almacen [Combustible, Comida]), (Motor 281)] ["emilia", "javier", "martina"]) EmptyT EmptyT))

--------------------------------------------------------------------------------------------------------------------------------

-- EJERCICIO 3.1

sectores :: Nave -> [SectorId]
-- PRECOND: Ninguna.
sectores (N ts) = sectoresDeTS ts

sectoresDeTS :: Tree Sector -> [SectorId]
-- PRECOND: Ninguna.
sectoresDeTS EmptyT            = []
sectoresDeTS (NodeT s ts1 ts2) = sectorIdDe s : sectoresDeTS ts1 ++ sectoresDeTS ts2

sectorIdDe :: Sector -> SectorId
-- PRECOND: Ninguna.
sectorIdDe (S id _ _) = id


-- EJERCICIO 3.2

poderDePropulsion :: Nave -> Int
-- PRECOND: Ninguna.
poderDePropulsion (N ts) = propulsionDeTS ts

propulsionDeTS :: Tree Sector -> Int
-- PRECOND: Ninguna.
propulsionDeTS EmptyT            = 0
propulsionDeTS (NodeT s ts1 ts2) = propulsionDeS s + propulsionDeTS ts1 + propulsionDeTS ts2

propulsionDeS :: Sector -> Int
-- PRECOND: Ninguna.
propulsionDeS (S sid comp trip) = propulsionDeComp comp

propulsionDeComp :: [Componente] -> Int
-- PRECOND: Ninguna.
propulsionDeComp []     = 0
propulsionDeComp (c:cs) = propulsionDeC c + propulsionDeComp cs

propulsionDeC :: Componente -> Int
-- PRECOND: Ninguna.
propulsionDeC (Motor n) = n
propulsionDeC _         = 0


-- EJERCICIO 3.3

barriles :: Nave -> [Barril]
-- PRECOND: Ninguna.
barriles (N ts) = barrilesDeTS ts

barrilesDeTS :: Tree Sector -> [Barril]
-- PRECOND: Ninguna.
barrilesDeTS EmptyT            = []
barrilesDeTS (NodeT s ts1 ts2) = barrilesDeS s ++ barrilesDeTS ts1 ++ barrilesDeTS ts2

barrilesDeS :: Sector -> [Barril]
-- PRECOND: Ninguna.
barrilesDeS (S sid comp trip) = barrilesDeComp comp

barrilesDeComp :: [Componente] -> [Barril]
-- PRECOND: Ninguna.
barrilesDeComp []     = []
barrilesDeComp (c:cs) = barrilesDeC c ++ barrilesDeComp cs

barrilesDeC :: Componente -> [Barril]
-- PRECOND: Ninguna.
barrilesDeC (Almacen b) = b
barrilesDeC _           = []


-- EJERCICIO 3.4

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
-- PRECOND: Ninguna.
agregarASector c s (N ts) = N (agregarCompASectorEnNave c s ts)

agregarCompASectorEnNave :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
-- PRECOND: Ninguna.
agregarCompASectorEnNave comp sec EmptyT            = EmptyT
agregarCompASectorEnNave comp sec (NodeT s ts1 ts2) = NodeT (agregarCompSiEsSector comp sec s)
                                                            (agregarCompASectorEnNave comp sec ts1)
                                                            (agregarCompASectorEnNave comp sec ts2)

agregarCompSiEsSector :: [Componente] -> SectorId -> Sector -> Sector
-- PRECOND: Ninguna.
agregarCompSiEsSector comp sec s = if esMismoSectorId sec s
                                      then agregarCompASector comp s
                                      else s

esMismoSectorId :: SectorId -> Sector -> Bool
-- PRECOND: Ninguna.
esMismoSectorId sec (S sid comp trip) = sec == sid

agregarCompASector :: [Componente] -> Sector -> Sector
-- PRECOND: Ninguna.
agregarCompASector c (S sid comp trip) = S sid (comp ++ c) trip


-- EJERCICIO 3.5

asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
-- PRECOND: Todos los ID de la lista existen en la nave.
asignarTripulanteA t s (N ts) = N (asignarTripulanteATS t s ts)

asignarTripulanteATS :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
-- PRECOND: Ninguna.
asignarTripulanteATS trip sid EmptyT            = EmptyT
asignarTripulanteATS trip sid (NodeT s ts1 ts2) = NodeT (agregarTripulanteAlSectorSiEsS trip sid s)
                                                        (asignarTripulanteATS trip sid ts1)
                                                        (asignarTripulanteATS trip sid ts2)

agregarTripulanteAlSectorSiEsS :: Tripulante -> [SectorId] -> Sector -> Sector
-- PRECOND: Ninguna.
agregarTripulanteAlSectorSiEsS trip sids sec = if estaEnIdsElId sids (idDelSector sec)
                                                  then agregarTripulanteASector trip sec
                                                  else sec

estaEnIdsElId :: [SectorId] -> SectorId -> Bool
-- PRECOND: Ninguna.
estaEnIdsElId []     sid = False
estaEnIdsElId (s:ss) sid = s == sid || estaEnIdsElId ss sid

idDelSector :: Sector -> SectorId
-- PRECOND: Ninguna.
idDelSector (S sid comp trip) = sid

agregarTripulanteASector :: Tripulante -> Sector -> Sector
-- PRECOND: Ninguna.
agregarTripulanteASector t (S sid c ts) = S sid c (t:ts)


-- EJERCICIO 3.6

sectoresAsignados :: Tripulante -> Nave -> [SectorId]
-- PRECOND: Ninguna.
sectoresAsignados t (N ts) = sectoresAsignadosDeT t ts

sectoresAsignadosDeT :: Tripulante -> Tree Sector -> [SectorId]
-- PRECOND: Ninguna.
sectoresAsignadosDeT t EmptyT            = []
sectoresAsignadosDeT t (NodeT s ts1 ts2) = sectorIdSiEstaTripulanteEn t s ++ sectoresAsignadosDeT t ts1 ++ sectoresAsignadosDeT t ts2

sectorIdSiEstaTripulanteEn :: Tripulante -> Sector -> [SectorId]
-- PRECOND: Ninguna.
sectorIdSiEstaTripulanteEn t (S sid comp ts) = if estaTripulanteEn t ts
                                                  then [sid]
                                                  else []

estaTripulanteEn :: Tripulante -> [Tripulante] -> Bool
-- PRECOND: Ninguna.
estaTripulanteEn trip []     = False
estaTripulanteEn trip (t:ts) = trip == t || estaTripulanteEn trip ts


-- EJERCICIO 3.7

tripulantes :: Nave -> [Tripulante]
-- PRECOND: Ninguna.
tripulantes (N ts) = tripulantesSinRepetidos (tripulantesDeTS ts)

tripulantesDeTS :: Tree Sector -> [Tripulante]
-- PRECOND: Ninguna.
tripulantesDeTS EmptyT            = []
tripulantesDeTS (NodeT s ts1 ts2) = tripulantesDeS s ++ tripulantesDeTS ts1 ++ tripulantesDeTS ts2

tripulantesDeS :: Sector -> [Tripulante]
-- PRECOND: Ninguna.
tripulantesDeS (S sid comp trip) = trip

tripulantesSinRepetidos :: [Tripulante] -> [Tripulante]
tripulantesSinRepetidos []     = []
tripulantesSinRepetidos (x:xs) = if noSeRepiteEn x xs
                                    then x : tripulantesSinRepetidos xs
                                    else tripulantesSinRepetidos xs

noSeRepiteEn :: Eq a => a -> [a] -> Bool
noSeRepiteEn x []     = True
noSeRepiteEn x (y:ys) = x/=y && noSeRepiteEn x ys


-- PUNTO 4 (Manada de Lobos):

type Presa      = String -- Nombre de presa

type Territorio = String -- Nombre de territorio

type Nombre     = String -- Nombre de lobo

data Lobo       = Cazador    Nombre [Presa]      Lobo Lobo Lobo
                | Explorador Nombre [Territorio] Lobo Lobo
                | Cria       Nombre
    deriving Show

data Manada = M Lobo
    deriving Show

----------------------------------------- FUNCIONES DE PRUEBA -----------------------------------------



-------------------------------------------------------------------------------------------------------

-- EJERCICIO 4.1

manadaTemible :: Manada
manadaTemible = M (Cazador "Carlitos" ["Gallina", "Cabra", "Mamut Australiano"]
                        (Cria "Carlitos JR")
                        (Explorador "Tadeo" ["La Pampa", "Misiones", "Quilmes"]
                            (Cria "Zell") (Explorador "Gallardo" ["La Pampa", "Belgrano"]
                                                (Cria "Borja")
                                                (Cria "Lanzini")))
                        (Cria "FabiDeRamos")
                   )

-- EJERCICIO 4.2

buenaCaza :: Manada -> Bool
-- PRECOND: Ninguna.
buenaCaza (M l) = cantidadDeAlimentoDeL l > cantidadDeCriasDeL l

cantidadDeAlimentoDeL :: Lobo -> Int
-- PRECOND: Ninguna.
cantidadDeAlimentoDeL (Cria n)                = 0
cantidadDeAlimentoDeL (Explorador n ts l1 l2) = 0 + cantidadDeAlimentoDeL l1 + cantidadDeAlimentoDeL l2
cantidadDeAlimentoDeL (Cazador n ps l1 l2 l3) = cantidadDePresasPS ps    + cantidadDeAlimentoDeL l1
                                              + cantidadDeAlimentoDeL l2 + cantidadDeAlimentoDeL l3

cantidadDePresasPS :: [Presa] -> Int
-- PRECOND: Ninguna.
cantidadDePresasPS []     = 0
cantidadDePresasPS (p:ps) = 1 + cantidadDePresasPS ps

cantidadDeCriasDeL :: Lobo -> Int
-- PRECOND: Ninguna.
cantidadDeCriasDeL (Cria n)                = 1
cantidadDeCriasDeL (Explorador n ts l1 l2) = cantidadDeCriasDeL l1 + cantidadDeCriasDeL l2
cantidadDeCriasDeL (Cazador n ps l1 l2 l3) = cantidadDeCriasDeL l1 + cantidadDeCriasDeL l2 + cantidadDeCriasDeL l3


-- EJERCICIO 4.3

elAlfa :: Manada -> (Nombre, Int)
-- PRECOND: Ninguna.
elAlfa (M l) = elAlfaDeL l

elAlfaDeL :: Lobo -> (Nombre, Int)
-- PRECOND: Ninguna.
elAlfaDeL (Cria n)                = (n, 0)
elAlfaDeL (Explorador n ts l1 l2) = elAlfaEntre (n, 0) (elAlfaEntre  (elAlfaDeL l1) (elAlfaDeL l2))
elAlfaDeL (Cazador n ps l1 l2 l3) = elAlfaEntre (elAlfaEntre (n, cantidadDePresasPS ps) (elAlfaDeL l1))
                                                (elAlfaEntre (elAlfaDeL l2)             (elAlfaDeL l3))

elAlfaEntre :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
-- PRECOND: Ninguna.
elAlfaEntre (nom1, n1) (nom2, n2) = if n1 > n2
                                       then (nom1, n1)
                                       else (nom2, n2)


-- EJERCICIO 4.4

losQueExploraron :: Territorio -> Manada -> [Nombre]
-- PRECOND: Ninguna.
losQueExploraron t (M l) = losQueExploraronL t l

losQueExploraronL :: Territorio -> Lobo -> [Nombre]
-- PRECOND: Ninguna.
losQueExploraronL t (Cria n)                = []
losQueExploraronL t (Explorador n ts l1 l2) = nombreSiExploroEn n t ts ++ losQueExploraronL t l1 ++ losQueExploraronL t l2
losQueExploraronL t (Cazador n ps l1 l2 l3) = losQueExploraronL t l1   ++ losQueExploraronL t l2 ++ losQueExploraronL t l3

nombreSiExploroEn :: Nombre -> Territorio -> [Territorio] -> [Nombre]
-- PRECOND: Ninguna.
nombreSiExploroEn n t ts = if recorrioTerritorio t ts
                              then [n]
                              else []

recorrioTerritorio :: Territorio -> [Territorio] -> Bool
-- PRECOND: Ninguna.
recorrioTerritorio ter []     = False
recorrioTerritorio ter (t:ts) = esTerritorio ter t || recorrioTerritorio ter ts

esTerritorio :: Territorio -> Territorio -> Bool
-- PRECOND: Ninguna.
esTerritorio t1 t2 = t1 == t2


-- EJERCICIO 4.5

-- exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
-- -- PRECOND: Ninguna.
-- exploradoresPorTerritorio (M l) = exploradoresPorTerritorioL l
-- 
-- exploradoresPorTerritorioL :: Lobo -> [(Territorio, [Nombre])]
-- -- PRECOND: Ninguna.
-- exploradoresPorTerritorioL (Cria n)                = []
-- exploradoresPorTerritorioL (Explorador n ts l1 l2) = n ... ts ... exploradoresPorTerritorioL l1 ... exploradoresPorTerritorioL l2
-- exploradoresPorTerritorioL (Cazador n ps l1 l2 l3) = n ... ps ... exploradoresPorTerritorioL l1 ... exploradoresPorTerritorioL l2 ... exploradoresPorTerritorioL l3

loberioFeroz :: Manada
loberioFeroz = M (Cazador "Colmillonazo" ["Despistadeo", "Perezosandra", "Papanatalia"]
                    (Cria "Chiquilin")
                    (Explorador "Astutobias" ["Parque Yellowstone", "Rio Mojado"]
                                (Cria "Enanin") (Explorador "Astutomas" ["Bosque Verdoso", "Rio Azulado"]
                                                            (Cria "Petisin") (Cazador "Colmillonatan" ["Despistadeo", "Perezosandra", "Papanatalia"]
                                                                                       (Cria "Chiquilin") (Cria "Chiquilin") (Cria "Chiquilin"))))
                    (Cria "Chiquitin"))


manadaEj :: Manada
manadaEj = M (Cazador "DienteFiloso" ["Búfalos", "Antílopes"]
                (Cría "Hopito")
                (Explorador "Incansable" ["Oeste hasta el río"]
                            (Cría "MechónGris") 
                            (Cría "Rabito"))
                (Cazador "Garras" ["Antílopes", "Ciervos"]
                    (Explorador "Zarpado" ["Bosque este"]
                        (Cría "Osado")
                        (Cazador "Mandíbulas" ["Cerdos", "Pavos"]
                            (Cría "Desgreñado")
                            (Cría "Malcriado")
                            (Cazador "TrituraHuesos" ["Conejos"]
                                (Cría "Peludo")
                                (Cría "Largo")
                                (Cría "Menudo")
                            )
                        )
                    )
                    (Cría "Garrita")
                    (Cría "Manchas")
                ))


-- EJERCICIO 4.6

cazadoresSuperioresDe :: Nombre -> Manada -> [Nombre]
-- PRECOND: Hay un lobo con dicho nombre y es único.
cazadoresSuperioresDe n (M l) = cazadoresSuperioresDeL n l

cazadoresSuperioresDeL :: Nombre -> Lobo -> [Nombre]
-- PRECOND: Ninguna.
cazadoresSuperioresDeL n (Cria n)                = ... n
cazadoresSuperioresDeL n (Explorador n ts l1 l2) = ... n ... ts ... cazadoresSuperioresDeL l1 ... cazadoresSuperioresDeL l2
cazadoresSuperioresDeL n (Cazador n ps l1 l2 l3) = ... n ... ps ... cazadoresSuperioresDeL l1 ... cazadoresSuperioresDeL l2 ... cazadoresSuperioresDeL l3