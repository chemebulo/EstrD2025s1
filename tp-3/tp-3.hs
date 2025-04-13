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
sumarT (NodeT n n1 n2) = n + sumarT n1 + sumarT n2


-- EJERCICIO 2.2

sizeT :: Tree a -> Int
-- PRECOND: Ninguna.
sizeT EmptyT          = 0
sizeT (NodeT x t1 t2) = 1 + sizeT t1 + sizeT t2


-- EJERCICIO 2.3

mapDobleT :: Tree Int -> Tree Int
-- PRECOND: Ninguna.
mapDobleT EmptyT          = EmptyT
mapDobleT (NodeT n n1 n2) = NodeT (n*2) (mapDobleT n1) (mapDobleT n2)


-- EJERCICIO 2.4

perteneceT :: Eq a => a -> Tree a -> Bool
-- PRECOND: Ninguna.
perteneceT x EmptyT          = False
perteneceT x (NodeT y t1 t2) = x == y || perteneceT x t1 || perteneceT x t2


-- EJERCICIO 2.5

aparicionesT :: Eq a => a -> Tree a -> Int
-- PRECOND: Ninguna.
aparicionesT x EmptyT          = 0
aparicionesT x (NodeT y t1 t2) = unoSi (x == y) + aparicionesT x t1 + aparicionesT x t2


-- EJERCICIO 2.6

leaves :: Tree a -> [a]
-- PRECOND: Ninguna.
leaves EmptyT          = []
leaves (NodeT x t1 t2) = singularSi x (esHoja t1 t2)  ++ leaves t1 ++ leaves t2

singularSi :: a -> Bool -> [a]
-- PRECOND: Ninguna.
singularSi x True  = [x]
singularSi x False = []

esHoja :: Tree a -> Tree a -> Bool
-- PRECOND: Ninguna.
esHoja EmptyT EmptyT = True
esHoja _      _      = False


-- EJERCICIO 2.7

heightT :: Tree a -> Int
-- PRECOND: Ninguna.
heightT EmptyT          = 0
heightT (NodeT x t1 t2) = 1 + max (heightT t1) (heightT t2)


-- EJERCICIO 2.8

mirrorT :: Tree a -> Tree a
-- PRECOND: Niguna.
mirrorT EmptyT          = EmptyT
mirrorT (NodeT x t1 t2) = NodeT x (mirrorT t2) (mirrorT t1)


-- EJERCICIO 2.9

toList :: Tree a -> [a]
-- PRECOND: Ninguna.
toList EmptyT          = []
toList (NodeT x t1 t2) = toList t1 ++ [x] ++ toList t2


-- EJERCICIO 2.10

levelN :: Int -> Tree a -> [a]
-- PRECOND: Ninguna.
levelN _ EmptyT          = []
levelN 0 (NodeT x t1 t2) = [x]
levelN n (NodeT x t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2


-- EJERCICIO 2.11

listPerLevel :: Tree a -> [[a]]
-- PRECOND: Ninguna.
listPerLevel EmptyT          = []
listPerLevel (NodeT x t1 t2) = [x] : unirNivelesDe (listPerLevel t1) (listPerLevel t2)

unirNivelesDe :: [[a]] -> [[a]] -> [[a]]
-- PRECOND: Ninguna.
unirNivelesDe xss     []      = xss
unirNivelesDe []      yss     = yss
unirNivelesDe (xs:xss) (ys:yss)  = (xs ++ ys) : unirNivelesDe xss yss


-- EJERCICIO 2.12

ramaMasLarga :: Tree a -> [a]
-- PRECOND: Ninguna.
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x t1 t2) = x : ramaMasLargaEntre (ramaMasLarga t1) (ramaMasLarga t2)

ramaMasLargaEntre :: [a] -> [a] -> [a]
-- PRECOND: Ninguna.
ramaMasLargaEntre xs ys = if length xs > length  ys
                             then xs
                             else ys


-- EJERCICIO 2.13

todosLosCaminos :: Tree a -> [[a]]
-- PRECOND: Ninguna.
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT x t1 t2) = [x] : consACadaDe x (todosLosCaminos t1 ++ todosLosCaminos t2)

consACadaDe :: a -> [[a]] -> [[a]]
-- PRECOND: Ninguna.
consACadaDe x []       = []
consACadaDe x (ys:yss) = (x:ys) : consACadaDe x yss


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

data ExpA = Valor Int
          | Sum   ExpA ExpA
          | Prod  ExpA ExpA
          | Neg   ExpA
    deriving Show

-- EJERCICIO 2.1

eval :: ExpA -> Int
-- PRECOND: Ninguna.
eval (Valor n)     = n
eval (Sum   e1 e2) = eval e1 + eval e2
eval (Prod  e1 e2) = eval e1 * eval e2
eval (Neg   e)     = eval e * (-1)


-- EJERCICIO 2.2

simplificar :: ExpA -> ExpA
simplificar (Valor e)     = Valor e
simplificar (Sum   e1 e2) = simplificarSuma (simplificar e1) (simplificar e2) 
simplificar (Prod  e1 e2) = simplificarMultiplicacion (simplificar e1) (simplificar e2)
simplificar (Neg   e)     = simplificarNegativo (simplificar e)

simplificarSuma :: ExpA -> ExpA -> ExpA
-- PRECOND: Ninguna.
simplificarSuma (Valor 0) e2        = e2
simplificarSuma e1        (Valor 0) = e1
simplificarSuma exp1      exp2      = Sum exp1 exp2

simplificarMultiplicacion :: ExpA -> ExpA -> ExpA
-- PRECOND: Ninguna.
simplificarMultiplicacion (Valor 0) e2        = Valor 0
simplificarMultiplicacion e1        (Valor 0) = Valor 0
simplificarMultiplicacion e1        (Valor 1) = e1
simplificarMultiplicacion (Valor 1) e2        = e2
simplificarMultiplicacion e1        e2        = Prod e1 e2

simplificarNegativo :: ExpA -> ExpA
-- PRECOND: Ninguna.
simplificarNegativo (Neg e) = e
simplificarNegativo exp     = Neg exp


----------------------------------------- FUNCIONES DE PRUEBA -----------------------------------------

expresion1 :: ExpA
expresion1 = Valor 10

expresion2 :: ExpA
expresion2 = Prod (Neg (Sum (Valor 10) (Valor 20))) (Neg (Valor 10)) -- Resultado: 300

expresion3 :: ExpA
expresion3 = Sum (Prod (Sum (Valor 15) (Valor 10)) (Neg (Neg (Valor 2)))) (Valor 5) -- Resultado: -45

-------------------------------------------------------------------------------------------------------