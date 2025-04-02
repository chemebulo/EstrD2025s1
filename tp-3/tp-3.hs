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



pasosHastaElTesoro :: Camino -> Int
-- PRECOND: Tiene que haber al menos un tesoro.
pasosHastaElTesoro (Nada cam)      = 1 + pasosHastaElTesoro cam
pasosHastaElTesoro (Cofre obj cam) = if hayTesoroEnObjetos obj
                                         then 0
                                         else 1 + pasosHastaElTesoro cam


---------------------------------------------------- FUNCIONES DE PRUEBA -------------------------------------------------------

camino0 :: Camino
camino0 = Nada (Nada (Cofre [Cacharro, Cacharro, Tesoro] (Nada Fin)))

camino1 :: Camino
camino1 = Nada (Nada (Cofre [Tesoro, Cacharro, Cacharro] Fin))

camino2 :: Camino
camino2 = Nada Fin

{-  
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
-}

--------------------------------------------------------------------------------------------------------------------------------


-- PUNTO 2: Tipos Arbóleos.

-- EJERCICIO 2.1 (Árboles Binarios):



-- EJERCICIO 2.2 (Expresiones Aritméticas):

