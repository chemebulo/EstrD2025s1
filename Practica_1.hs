-- PUNTO 2: Números Enteros. 

-- EJERCICIO 1, A:

sucesor :: Int -> Int
-- PRECOND: Ninguna.
sucesor x = x + 1


-- EJERCICIO 1, B:

sumar :: Int -> Int -> Int
-- PRECOND: Ninguna. 
sumar x y = x + y


-- EJERCICIO 1, C:

divisionYResto :: Int -> Int -> (Int, Int)
-- PRECOND: El valor del parámetro "y" tiene que ser diferente de cero.
divisionYResto x y = (div x y, mod x y)


-- EJERCICIO 1, D (Usando Alternativa condicional):

maxDelPar :: (Int, Int) -> Int
-- PRECOND: Ninguna.
maxDelPar (x, y) = if (x >= y)
                   then x
                   else y
                   {- Una extensión del Visual Studio me recomienda usar la función max, 
                      pero yo preferí hacerlo con Alternativa condicional. -}


-- EJERCICIO 2, EJEMPLOS:
    -- EJEMPLO 1: sucesor (sumar (maxDelPar (divisionYResto 5 3)) 7)
    -- EJEMPLO 2: sumar (maxDelPar (divisionYResto 10 5)) (sucesor 7)
    -- EJEMPLO 3: maxDelPar (divisionYResto (sucesor 9) (sumar 0 1))
    -- EJEMPLO 4: sumar (sucesor (maxDelPar (divisionYResto 8 2))) 5


-- PUNTO 3: Tipos enumerativos.

data Dir = Norte | Sur | Este | Oeste
    deriving Show


-- EJERCICIO 1, A:

opuesto :: Dir -> Dir
-- PRECOND: Ninguna.
opuesto Norte = Sur
opuesto Este  = Oeste
opuesto Sur   = Norte
opuesto Oeste = Este


-- EJERCICIO 1, B:

iguales :: Dir -> Dir -> Bool
-- PRECOND: Ninguna.
iguales Norte Norte = True
iguales Este Este   = True
iguales Sur Sur     = True
iguales Oeste Oeste = True
iguales _ _         = False
-- ¿Habrá otra forma de hacer este ejercicio?


-- EJERCICIO 1, C:

siguiente :: Dir -> Dir
-- PRECOND: El parámetro no puede usar Oeste como valor.
siguiente Norte = Este
siguiente Este  = Sur
siguiente Sur   = Oeste
{- Entiendo que es una función parcial ya que una de las posibles direcciones 
   a utilizar (Oeste) no es tenida en cuenta como posible valor, por ende, debe 
   quedar explícita tal restricción para no dar lugar a un error involuntario en la invocación. -}


-- EJERCICIO 2, A:

data DiaDeSemana = Lunes | Martes | Miércoles | Jueves | Viernes | Sábado | Domingo
    deriving Show

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
-- PRECOND: Ninguna.
primeroYUltimoDia = (primerDiaDeSemana, ultimoDiaDeSemana)

primerDiaDeSemana :: DiaDeSemana
-- PRECOND: Ninguna.
primerDiaDeSemana = Lunes

ultimoDiaDeSemana :: DiaDeSemana
-- PRECOND: Ninguna.
ultimoDiaDeSemana = Domingo


-- EJERCICIO 2, B:

empiezaConM :: DiaDeSemana -> Bool
-- PRECOND: Ninguna.
empiezaConM Martes    = True
empiezaConM Miércoles = True
empiezaConM _         = False


-- EJERCICIO 2, C:

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
-- PRECOND: Ninguna.
vieneDespues dia1 dia2 = numeroDeDia dia1 > numeroDeDia dia2

numeroDeDia :: DiaDeSemana -> Int
-- PRECOND: Ninguna.
numeroDeDia Lunes     = 1
numeroDeDia Martes    = 2
numeroDeDia Miércoles = 3
numeroDeDia Jueves    = 4
numeroDeDia Viernes   = 5
numeroDeDia Sábado    = 6
numeroDeDia Domingo   = 7


-- EJERCICIO 2, D:

estaEnElMedio :: DiaDeSemana -> Bool
-- PRECOND: Ninguna.
estaEnElMedio Lunes   = False
estaEnElMedio Domingo = False
estaEnElMedio _       = True


-- EJERCICIO 3, A:

negar :: Bool -> Bool
-- PRECOND: Ninguna.
negar True = False
negar False = True


-- EJERCICIO 3, B:

implica :: Bool -> Bool -> Bool
-- PRECOND: Ninguna.
implica b1 b2 = not (esTrueYFalse b1 b2)

esTrueYFalse :: Bool -> Bool -> Bool
-- PRECOND: Ninguna.
esTrueYFalse True False = True
esTrueYFalse _ _        = False


-- EJERCICIO 3, C:

yTambien :: Bool -> Bool -> Bool
-- PRECOND: Ninguna.
yTambien b1 b2 = esTrueYTrue b1 b2

esTrueYTrue :: Bool -> Bool -> Bool
-- PRECOND: Ninguna.
esTrueYTrue True True = True
esTrueYTrue _ _       = False


-- EJERCICIO 3, D:

oBien :: Bool -> Bool -> Bool
-- PRECOND: Ninguna.
oBien b1 b2 = algunoEsTrue b1 b2

algunoEsTrue :: Bool -> Bool -> Bool
-- PRECOND: Ninguna.
algunoEsTrue True _ = True
algunoEsTrue _ True = True
algunoEsTrue _ _    = False