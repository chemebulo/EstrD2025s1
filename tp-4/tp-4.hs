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




-- EJERCICIO 1.2




-- EJERCICIO 1.3




-- EJERCICIO 1.4




-- EJERCICIO 1.5




-- EJERCICIO 1.6




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




-- EJERCICIO 2.2




-- EJERCICIO 2.3




-- EJERCICIO 2.4




-- EJERCICIO 2.5




-- EJERCICIO 2.6




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




-- EJERCICIO 3.2




-- EJERCICIO 3.3




-- EJERCICIO 3.4




-- EJERCICIO 3.5




-- EJERCICIO 3.6




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




-- EJERCICIO 4.3




-- EJERCICIO 4.4




-- EJERCICIO 4.5




-- EJERCICIO 4.6

