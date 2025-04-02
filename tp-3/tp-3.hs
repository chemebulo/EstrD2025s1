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
-- PRECOND: Ninguna.
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




-- EJERCICIO 2.2 (Expresiones Aritméticas):

