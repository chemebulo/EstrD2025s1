-- PUNTO 1: Tipos Recursivos Simples.

-- EJERCICIO 1.1 (Celdas con bolitas):

data Color = Azul | Rojo
    deriving Show

data Celda = Bolita Color Celda | CeldaVacia
    deriving Show


nroBolitas :: Color -> Celda -> Int
-- PRECOND: Ninguna.
nroBolitas _ CeldaVacia      = 0
nroBolitas c1 (Bolita c2 cel) = unoSiEsBolitaDeColor c2 c1 + nroBolitas c1 cel

unoSiEsBolitaDeColor :: Color -> Color -> Int
-- PRECOND: Ninguna.
unoSiEsBolitaDeColor Azul Azul = 1
unoSiEsBolitaDeColor Rojo Rojo = 1
unoSiEsBolitaDeColor _    _    = 0


---------------------------------------------------- FUNCIONES DE PRUEBA -------------------------------------------------------

celda0 :: Celda
celda0 = CeldaVacia

celda1 :: Celda
celda1 = Bolita Azul CeldaVacia

celda2 :: Celda
celda2 = Bolita Rojo (Bolita Azul CeldaVacia)

celda3 :: Celda
celda3 = Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia))

celda4 :: Celda
celda4 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia))) -- Tiene 2 bolitas azules y 2 rojas.

--------------------------------------------------------------------------------------------------------------------------------


-- EJERCICIO 1.2 (Camino hacia el tesoro):

data Objeto = Cacharro | Tesoro
    deriving Show

data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show



-- PUNTO 2: Tipos Arbóleos.

-- EJERCICIO 2.1 (Árboles Binarios):



-- EJERCICIO 2.2 (Expresiones Aritméticas):