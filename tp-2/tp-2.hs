-- PUNTO 1: Recursión sobre listas.

-- EJERCICIO 1.1:

sumatoria :: [Int] -> Int
-- PRECOND: Ninguna.
sumatoria []     = 0
sumatoria (n:ns) = n + sumatoria ns


-- EJERCICIO 1.2:

longitud :: [a] -> Int
-- PRECOND: Ninguna.
longitud []     = 0
longitud (x:xs) = 1 + longitud xs


-- EJERCICIO 1.3:

sucesores :: [Int] -> [Int]
-- PRECOND: Ninguna.
sucesores []     = []
sucesores (n:ns) = n+1 : sucesores ns


-- EJERCICIO 1.4:

conjuncion :: [Bool] -> Bool
-- PRECOND: Ninguna.
conjuncion []     = True
conjuncion (b:bs) = b && conjuncion bs


-- EJERCICIO 1.5:

disyuncion :: [Bool] -> Bool
-- PRECOND: Ninguna.
disyuncion []     = False
disyuncion (b:bs) = b || disyuncion bs


-- EJERCICIO 1.6:

aplanar :: [[a]] -> [a]
-- PRECOND: Ninguna.
aplanar []     = []
aplanar (x:xs) = x ++ aplanar xs


-- EJERCICIO 1.7:

pertenece :: Eq a => a -> [a] -> Bool
-- PRECOND: Ninguna.
pertenece e []     = False
pertenece e (x:xs) = e == x || pertenece e xs


-- EJERCICIO 1.8:

apariciones :: Eq a => a -> [a] -> Int
-- PRECOND: Ninguna.
apariciones e []     = 0
apariciones e (x:xs) = if e == x
                       then 1 + apariciones e xs
                       else apariciones e xs


-- EJERCICIO 1.9:

losMenoresA :: Int -> [Int] -> [Int]
-- PRECOND: Ninguna.
losMenoresA n []     = []
losMenoresA n (ns:nss) = if ns < n
                         then ns : losMenoresA n nss
                         else losMenoresA n nss


-- EJERCICIO 1.10:

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
-- PRECOND: El número dado debe ser mayor o igual a 0.
lasDeLongitudMayorA n []     = []
lasDeLongitudMayorA n (x:xs) = if longitud x > n
                               then x : lasDeLongitudMayorA n xs
                               else lasDeLongitudMayorA n xs


-- EJERCICIO 1.11:

agregarAlFinal :: [a] -> a -> [a]
-- PRECOND: Ninguna.
agregarAlFinal []     y = [y]
agregarAlFinal (x:xs) y = x : agregarAlFinal xs y


-- EJERCICIO 1.12:

agregar :: [a] -> [a] -> [a]
-- PRECOND: Ninguna.
agregar []     []     = []
agregar (x:xs) []     = x:xs
agregar []     (y:ys) = y:ys
agregar (x:xs) (y:ys) = x : agregar xs (y:ys)


-- EJERCICIO 1.13:

reversa :: [a] -> [a]
-- PRECOND: Ninguna.
reversa []     = []
reversa (x:xs) = agregar (reversa xs) [x]


-- EJERCICIO 1.14:

zipMaximos :: [Int] -> [Int] -> [Int]
-- PRECOND: Ninguna.
zipMaximos []     ys     = ys
zipMaximos xs     []     = xs
zipMaximos (x:xs) (y:ys) = if x >= y
                           then x : zipMaximos xs ys
                           else y : zipMaximos xs ys


-- EJERCICIO 1.15:

elMinimo :: Ord a => [a] -> a
-- PRECOND: La lista dada no puede ser vacía.
elMinimo (x:[])  =  x
elMinimo (x:xs) = if x < elMinimo xs
                  then x
                  else elMinimo xs


-- PUNTO 2: Recusión sobre números.


-- EJERCICIO 2.1:

factorial :: Int -> Int
-- PRECOND: El número dado debe ser mayor o igual a 0.
factorial 0 = 1
factorial n = n * factorial (n-1)


-- EJERCICIO 2.2:

cuentaRegresiva :: Int -> [Int]
-- PRECOND: El número dado debe ser mayor o igual a 0.
cuentaRegresiva n = if n >= 1
                    then n : cuentaRegresiva (n-1)
                    else []


-- EJERCICIO 2.3:

repetir :: Int -> a -> [a]
-- PRECOND: El número dado debe ser mayor o igual a 0.
repetir 0 e = []
repetir n e = e : repetir (n-1) e


-- EJERCICIO 2.4:

losPrimeros :: Int -> [a] -> [a]
-- PRECOND: El número dado debe ser mayor o igual a 0.
losPrimeros 0 _      = []
losPrimeros _ []     = []
losPrimeros n (x:xs) = x : (losPrimeros (n-1) xs)


-- EJERCICIO 2.5:

sinLosPrimeros :: Int -> [a] -> [a]
-- PRECOND: El número dado debe ser mayor o igual a 0.
sinLosPrimeros 0 xs     = xs
sinLosPrimeros _ []     = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs


-- PUNTO 3: Registros.


-- EJERCICIO 3.1:

data Persona = P String Int
              -- Nombre Edad
{- INV. REP:
    - El nombre no puede ser vacío.
    - La  edad debe tener un número mayor o igual a cero. 
-}
    deriving Show

mayoresA :: Int -> [Persona] -> [Persona]
-- PRECOND: El número debe ser mayor o igual a cero.
mayoresA 0 _      = []
mayoresA _ []     = []
mayoresA n (x:xs) = if 


-- EJERCICIO 3.2:

data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]


-- EJERCICIO 3.3:

data Seniority = Junior | SemiSenior | Senior
data Proyecto = ConsProyecto String
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
data Empresa = ConsEmpresa [Rol]

