-- PUNTO 1: Tipos Recursivos Simples.

-- EJERCICIO 1.1 (Celdas con bolitas):

data Color = Azul | Rojo
    deriving Show

data Celda = Bolita Color Celda | CeldaVacia
    deriving Show


nroBolitas :: Color -> Celda -> Int
-- PRECOND: Ninguna.
nroBolitas _  CeldaVacia      = 0
nroBolitas c1 (Bolita c2 cel) = unoSi (esElMismoColor c2 c1)  + nroBolitas c1 cel

unoSi :: Bool -> Int
-- PRECOND: Ninguna.
unoSi True  = 1
unoSi False = 0

esElMismoColor :: Color -> Color -> Bool
-- PRECOND: Ninguna.
esElMismoColor Azul Azul = True
esElMismoColor Rojo Rojo = True
esElMismoColor _    _    = False



poner :: Color -> Celda -> Celda
-- PRECOND: Ninguna.
poner c1 cel = Bolita c1 cel



sacar :: Color -> Celda -> Celda
-- PRECOND: Ninguna.
sacar _  CeldaVacia      = CeldaVacia
sacar c1 (Bolita c2 cel) = if esElMismoColor c1 c2
                              then cel
                              else Bolita c2 (sacar c1 cel)

-- Es cuestion de criterio utilizar o no una subtarea, si no es necesario, preferiblemente evitarlo. En caso de funciones más grandes que terminen generando un if 
-- más complejo es posible que SI sea necesario terminar desarrollando una subtarea, pero para este caso, no es necesario.



ponerN :: Int -> Color -> Celda -> Celda
-- PRECOND: El número es mayor o igual a 0.
ponerN 0 _   cel = cel 
ponerN n col cel = poner col (ponerN (n-1) col cel)



-- EJERCICIO 1.2 (Camino hacia el tesoro):

data Objeto = Cacharro | Tesoro
    deriving Show

data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show


hayTesoro :: Camino -> Bool
-- PRECOND: Ninguna.
hayTesoro Fin             = False
hayTesoro (Nada cam)      = hayTesoro cam
hayTesoro (Cofre obj cam) = hayTesoroEnObjetos obj || hayTesoro cam

hayTesoroEnObjetos :: [Objeto] -> Bool
-- PRECOND: Ninguna.
hayTesoroEnObjetos []     = False
hayTesoroEnObjetos (x:xs) = esTesoro x || hayTesoroEnObjetos xs

esTesoro :: Objeto -> Bool
-- PRECOND: Ninguna.
esTesoro Tesoro = True
esTesoro _      = False



pasosHastaTesoro :: Camino -> Int
-- PRECOND: Tiene que haber al menos un tesoro.
pasosHastaTesoro (Nada cam)      = 1 + pasosHastaTesoro cam
pasosHastaTesoro (Cofre obj cam) = if hayTesoroEnObjetos obj
                                      then 0
                                      else 1 + pasosHastaTesoro cam



hayTesoroEn :: Int -> Camino -> Bool
-- PRECOND: Ninguna.
hayTesoroEn n Fin             = False
hayTesoroEn n (Nada cam)      = hayTesoroEn (n-1) cam 
hayTesoroEn n (Cofre obj cam) = if n == 0
                                   then hayTesoroEnObjetos obj
                                   else hayTesoroEn (n-1) cam



alMenosNTesoros :: Int -> Camino -> Bool
-- PRECOND: Ninguna.
alMenosNTesoros n cam = cantidadDeTesorosEnCamino cam >= n

cantidadDeTesorosEnCamino :: Camino -> Int
-- PRECOND: Ninguna.
cantidadDeTesorosEnCamino Fin             = 0
cantidadDeTesorosEnCamino (Nada cam)      = cantidadDeTesorosEnCamino cam 
cantidadDeTesorosEnCamino (Cofre obj cam) = cantidadDeTesorosEnObjetos obj + cantidadDeTesorosEnCamino cam

cantidadDeTesorosEnObjetos :: [Objeto] -> Int   
-- PRECOND: Ninguna.
cantidadDeTesorosEnObjetos []     = 0
cantidadDeTesorosEnObjetos (x:xs) = unoSi (esTesoro x) + cantidadDeTesorosEnObjetos xs



-- DESAFÍO:

cantTesorosEntre :: Int -> Int -> Camino -> Int
-- PRECOND: El primer número debe ser menor o igual al segundo, y ambos deben ser mayor a 0.
cantTesorosEntre _  _  Fin = 0
cantTesorosEntre 0  n2 cam = cantTesorosHasta n2 cam
cantTesorosEntre n1 n2 cam = cantTesorosEntre (n1-1) (n2-1) (caminoDe cam)

cantTesorosHasta :: Int -> Camino -> Int
-- PRECOND: El número es mayor o igual a 0.
cantTesorosHasta 0 (Nada cam)      = 0
cantTesorosHasta n (Nada cam)      = cantTesorosHasta (n-1) cam
cantTesorosHasta 0 (Cofre obj cam) = cantidadDeTesorosEnObjetos obj
cantTesorosHasta n (Cofre obj cam) = cantidadDeTesorosEnObjetos obj + cantTesorosHasta (n-1) cam

caminoDe :: Camino -> Camino
-- PRECOND: Ninguna.
caminoDe Fin             = Fin
caminoDe (Nada cam)      = cam
caminoDe (Cofre obj cam) = cam



-- PUNTO 2: Tipos Arbóleos.

-- EJERCICIO 2.1 (Árboles Binarios):

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show


-- EJERCICIO 2.1

sumarT :: Tree Int -> Int
-- PRECOND: Ninguna.
sumarT EmptyT          = 0 
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2


-- EJERCICIO 2.2

sizeT :: Tree a -> Int
-- PRECOND: Ninguna.
sizeT EmptyT          = 0
sizeT (NodeT n t1 t2) = 1 + sizeT t1 + sizeT t2


-- EJERCICIO 2.3

mapDobleT :: Tree Int -> Tree Int
-- PRECOND: Ninguna.
mapDobleT EmptyT          = EmptyT
mapDobleT (NodeT n t1 t2) = NodeT (n*2) (mapDobleT t1) (mapDobleT t2)


-- EJERCICIO 2.4

perteneceT :: Eq a => a -> Tree a -> Bool
-- PRECOND: Ninguna.
perteneceT x EmptyT          = False
perteneceT x (NodeT n t1 t2) = x == n || perteneceT x t1 || perteneceT x t2 


-- EJERCICIO 2.5

aparicionesT :: Eq a => a -> Tree a -> Int
-- PRECOND: Ninguna.
aparicionesT x EmptyT          = 0 
aparicionesT x (NodeT n t1 t2) = unoSi (x == n) + aparicionesT x t1 + aparicionesT x t2


-- EJERCICIO 2.6

leaves :: Tree a -> [a]
-- PRECOND: Ninguna.
leaves EmptyT          = []
leaves (NodeT n t1 t2) = listaDeObjetoSiEsHoja n t1 t2 ++ leaves t1 ++ leaves t2

listaDeObjetoSiEsHoja :: a -> Tree a -> Tree a -> [a]
-- PRECOND: Ninguna.
listaDeObjetoSiEsHoja x EmptyT EmptyT = [x]
listaDeObjetoSiEsHoja x _      _      = []


-- EJERCICIO 2.7

heightT :: Tree a -> Int
-- PRECOND: Ninguna.
heightT EmptyT                  = 0
heightT (NodeT n EmptyT EmptyT) = 1
heightT (NodeT n t1 t2)         = max (profundidadDeRama t1) (profundidadDeRama t2)

profundidadDeRama :: Tree a -> Int
-- PRECOND: Ninguna.
profundidadDeRama EmptyT          = 0
profundidadDeRama (NodeT n t1 t2) = 1 + max (profundidadDeRama t1) (profundidadDeRama t2)


-- EJERCICIO 2.8



-- EJERCICIO 2.9

-- In order quiere decir de izquierda a derecha.

-- EJERCICIO 2.10




-- EJERCICIO 2.11




-- EJERCICIO 2.12


-- case d of 
-- Izq -> haces algo
-- Der -> haces algo


-- EJERCICIO 2.13




----------------------------------------- FUNCIONES DE PRUEBA -----------------------------------------

-- GRAFICO DIAPOSITIVA 36:

arbolStr :: Tree String
arbolStr = NodeT "E" nodoIzq nodoDer


nodoIzq :: Tree String
nodoIzq = NodeT "A" nodoIzqIzq nodoIzqDer

nodoIzqIzq :: Tree String
nodoIzqIzq = NodeT "M" EmptyT EmptyT

nodoIzqDer :: Tree String
nodoIzqDer = NodeT "O" EmptyT EmptyT


nodoDer :: Tree String
nodoDer = NodeT "M" EmptyT nodoDerIzq

nodoDerIzq :: Tree String
nodoDerIzq = NodeT "O" EmptyT EmptyT

{-
           "E"
         /    \
       "M"    "A"
        \     / \
        "O" "O" "M"
-}

-------------------------------------------------------------------------------------------------------

arbolInt :: Tree Int
arbolInt = NodeT 11 nodoIzq' nodoDer'


nodoIzq' :: Tree Int
nodoIzq' = NodeT 7 nodoIzqIzq' nodoIzqDer'

nodoIzqIzq' :: Tree Int
nodoIzqIzq' = NodeT 2 EmptyT EmptyT

nodoIzqDer' :: Tree Int
nodoIzqDer' = NodeT 5 EmptyT EmptyT


nodoDer' :: Tree Int
nodoDer' = NodeT 3 EmptyT nodoDerIzq'

nodoDerIzq' :: Tree Int
nodoDerIzq' = NodeT 0 EmptyT nodoDerIzqDer'

nodoDerIzqDer' :: Tree Int
nodoDerIzqDer' = NodeT 1 EmptyT EmptyT

{-
           11
         /    \
        3      7
        \     / \
         0   5   2
         /
        1
-}

-------------------------------------------------------------------------------------------------------


-- EJERCICIO 2.2 (Expresiones Aritméticas):

