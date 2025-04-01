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

{- VERSION 2:

¿Cuál de las dos versiones sigue lo planteado en la materia?

sacar :: Color -> Celda -> Celda
-- PRECOND: Ninguna.
sacar _  CeldaVacia      = CeldaVacia  
sacar c1 (Bolita c2 cel) = sacarBolitaSiSino cel (esElMismoColor c1 c2) (Bolita c2 (sacar c1 cel))

sacarBolitaSiSino :: Celda -> Bool -> Celda -> Celda
-- PRECOND: Ninguna.
sacarBolitaSiSino cel1 True  cel2 = cel1
sacarBolitaSiSino cel1 False cel2 = cel2

-}

-- Lo mismo para ponerN, ¿sigue los criteros adecuados?

ponerN :: Int -> Color -> Celda -> Celda
ponerN n c1 cel = if n > 0
                     then poner c1 (ponerN (n-1) c1 cel)
                     else cel



-- EJERCICIO 1.2 (Camino hacia el tesoro):

data Objeto = Cacharro | Tesoro
    deriving Show

data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show


-- Sin funcionar.

hayTesoro :: Camino -> Bool
-- PRECOND: Ninguna.
hayTesoro  Fin        = False
hayTesoro  c          = if esCofre c
                           then hayTesoroEnObjetos (objetosDeCofre c) || hayTesoro c
                           else hayTesoro c

esCofre :: Camino -> Bool
-- PRECOND: Ninguna.
esCofre (Cofre obj cam) = True
esCofre _               = False

objetosDeCofre :: Camino -> [Objeto]
-- PRECOND: Ninguna.
objetosDeCofre (Cofre obj cam) = obj
objetosDeCofre _               = []

hayTesoroEnObjetos :: [Objeto] -> Bool
-- PRECOND: Ninguna.
hayTesoroEnObjetos []     = False
hayTesoroEnObjetos (x:xs) = esTesoro x || hayTesoroEnObjetos xs

esTesoro :: Objeto -> Bool
-- PRECOND: Ninguna.
esTesoro Tesoro = True
esTesoro _      = False


camino0 :: Camino
camino0 = Fin

camino1 :: Camino
camino1 = Nada Fin

camino2 :: Camino
camino2 = Nada (Nada Fin)

camino3 :: Camino
camino3 = Cofre [Cacharro, Cacharro, Tesoro] (Nada (Nada Fin))

camino4 :: Camino
camino4 = Nada (Cofre [Cacharro, Cacharro, Tesoro] (Nada (Nada Fin)))


---------------------------------------------------- FUNCIONES DE PRUEBA -------------------------------------------------------



--------------------------------------------------------------------------------------------------------------------------------


-- PUNTO 2: Tipos Arbóleos.

-- EJERCICIO 2.1 (Árboles Binarios):



-- EJERCICIO 2.2 (Expresiones Aritméticas):