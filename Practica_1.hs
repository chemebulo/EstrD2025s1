-- PUNTO 2: Números Enteros. 

-- EJERCICIO 2.1, A:

sucesor :: Int -> Int
-- PRECOND: Ninguna.
sucesor x = x + 1


-- EJERCICIO 2.1, B:

sumar :: Int -> Int -> Int
-- PRECOND: Ninguna. 
sumar x y = x + y


-- EJERCICIO 2.1, C:

divisionYResto :: Int -> Int -> (Int, Int)
-- PRECOND: El valor del parámetro "y" tiene que ser diferente de cero.
divisionYResto x y = (div x y, mod x y)


-- EJERCICIO 2.1, D (Usando Alternativa condicional):

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


-- EJERCICIO 3.1, A:

opuesto :: Dir -> Dir
-- PRECOND: Ninguna.
opuesto Norte = Sur
opuesto Este  = Oeste
opuesto Sur   = Norte
opuesto Oeste = Este


-- EJERCICIO 3.1, B:

iguales :: Dir -> Dir -> Bool
-- PRECOND: Ninguna.
iguales Norte Norte = True
iguales Este Este   = True
iguales Sur Sur     = True
iguales Oeste Oeste = True
iguales _ _         = False
-- ¿Habrá otra forma de hacer este ejercicio?


-- EJERCICIO 3.1, C:

siguiente :: Dir -> Dir
-- PRECOND: El parámetro no puede usar Oeste como valor.
siguiente Norte = Este
siguiente Este  = Sur
siguiente Sur   = Oeste
{- Entiendo que es una función parcial ya que una de las posibles direcciones 
a utilizar (Oeste) no es tenida en cuenta como posible valor, por ende, debe 
quedar explícita tal restricción para no dar lugar a un error involuntario en la invocación. -}


-- EJERCICIO 3.2, A:

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
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


-- EJERCICIO 3.2, B:

empiezaConM :: DiaDeSemana -> Bool
-- PRECOND: Ninguna.
empiezaConM Martes    = True
empiezaConM Miercoles = True
empiezaConM _         = False


-- EJERCICIO 3.2, C:

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
-- PRECOND: Ninguna.
vieneDespues dia1 dia2 = numeroDeDia dia1 > numeroDeDia dia2

numeroDeDia :: DiaDeSemana -> Int
-- PRECOND: Ninguna.
numeroDeDia Lunes     = 1
numeroDeDia Martes    = 2
numeroDeDia Miercoles = 3
numeroDeDia Jueves    = 4
numeroDeDia Viernes   = 5
numeroDeDia Sabado    = 6
numeroDeDia Domingo   = 7


-- EJERCICIO 3.2, D:

estaEnElMedio :: DiaDeSemana -> Bool
-- PRECOND: Ninguna.
estaEnElMedio Lunes   = False
estaEnElMedio Domingo = False
estaEnElMedio _       = True


-- EJERCICIO 3.3, A:

negar :: Bool -> Bool
-- PRECOND: Ninguna.
negar True = False
negar False = True


-- EJERCICIO 3.3, B:

implica :: Bool -> Bool -> Bool
-- PRECOND: Ninguna.
implica b1 b2 = not (esTrueYFalse b1 b2)

esTrueYFalse :: Bool -> Bool -> Bool
-- PRECOND: Ninguna.
esTrueYFalse True False = True
esTrueYFalse _ _        = False


-- EJERCICIO 3.3, C:

yTambien :: Bool -> Bool -> Bool
-- PRECOND: Ninguna.
yTambien b1 b2 = esTrueYTrue b1 b2

esTrueYTrue :: Bool -> Bool -> Bool
-- PRECOND: Ninguna.
esTrueYTrue True True = True
esTrueYTrue _ _       = False


-- EJERCICIO 3.3, D:

oBien :: Bool -> Bool -> Bool
-- PRECOND: Ninguna.
oBien b1 b2 = algunoEsTrue b1 b2

algunoEsTrue :: Bool -> Bool -> Bool
-- PRECOND: Ninguna.
algunoEsTrue True _ = True
algunoEsTrue _ True = True
algunoEsTrue _ _    = False


-- PUNTO 4: Registros.

data Persona = P String Int
              -- Nombre Edad
    deriving Show

-- EJERCICIO 4.1:

nombre :: Persona -> String
-- PRECOND: Ninguna.
nombre (P n e) = n


edad :: Persona -> Int
-- PRECOND: Ninguna.
edad (P n e) = e


crecer :: Persona -> Persona
-- PRECOND: Ninguna.
crecer (P n e) = P n (e + 1)


cambioDeNombre :: String -> Persona -> Persona
-- PRECOND: Ninguna.
cambioDeNombre nom (P n e) = P nom e


esMayorQueLaOtra :: Persona -> Persona -> Bool
-- PRECOND: Ninguna.
esMayorQueLaOtra (P nom1 edad1) (P nom2 edad2) = edad (P nom1 edad1) > edad (P nom2 edad2)


laQueEsMayor :: Persona -> Persona -> Persona
-- PERCOND: Ninguna.
laQueEsMayor (P nom1 edad1) (P nom2 edad2) = if esMayorQueLaOtra (P nom1 edad1) (P nom2 edad2)
                                             then P nom1 edad1
                                             else P nom2 edad2


-- EJERCICIO 4.2:

data Pokemon = Poke TipoDePokemon Int
                               -- Porcentaje de energía.
    deriving Show


data Entrenador = Entr String Pokemon Pokemon
               -- Nombre Pokemon1 Pokemon2
    deriving Show


data TipoDePokemon = Agua | Fuego | Planta
    deriving Show


superaA :: Pokemon -> Pokemon -> Bool
-- PRECOND: Ninguna.
superaA (Poke tipo1 porcen1) (Poke tipo2 porcen2) = tipoDeEsSuperiorQue tipo1 tipo2

tipoDeEsSuperiorQue :: TipoDePokemon -> TipoDePokemon -> Bool
-- PRECOND: Ninguna.
tipoDeEsSuperiorQue Agua Fuego   = True
tipoDeEsSuperiorQue Fuego Planta = True
tipoDeEsSuperiorQue Planta Agua  = True
tipoDeEsSuperiorQue _ _          = False


cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
-- PRECOND: Ninguna.
cantidadDePokemonDe tipo (Entr nomb poke1 poke2) = (unoSiPokemonEsTipoCeroSino poke1 tipo) + (unoSiPokemonEsTipoCeroSino poke2 tipo)

unoSiPokemonEsTipoCeroSino :: Pokemon -> TipoDePokemon -> Int
-- PRECOND: Ninguna.
unoSiPokemonEsTipoCeroSino (Poke tipo1 porcen1) tipo2 = unoSiTipoEsMismoTipoQue tipo1 tipo2

unoSiTipoEsMismoTipoQue :: TipoDePokemon -> TipoDePokemon -> Int
-- PRECOND: Ninguna.
unoSiTipoEsMismoTipoQue Agua Agua     = 1
unoSiTipoEsMismoTipoQue Fuego Fuego   = 1
unoSiTipoEsMismoTipoQue Planta Planta = 1
unoSiTipoEsMismoTipoQue _ _           = 0


juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
-- PRECOND: Ninguna.
juntarPokemon ((Entr nom1 poke1 poke2), (Entr nom2 poke3 poke4)) = [poke1, poke2, poke3, poke4]


-- PUNTO 5: Funciones polimórficas.


-- EJERCICIO 5.1, A:

loMismo :: a -> a
-- PRECOND: Ninguna.
loMismo x = x


-- EJERCICIO 5.1, B:

siempreSiete :: a -> Int
-- PRECOND: Ninguna.
siempreSiete x = 7

-- EJERCICIO 5.1, C:

swap :: (a, b) -> (b, a)
-- PRECOND: Ninguna.
swap (x, y) = (y, x)
{- Existen dos tipos de variables diferentes en el par, ya que las tuplas son estructuras de datos que
   contienen 2 datos que no necesariamente deben ser del mismo tipo, y además, que sería imposible 
   diferenciar los componentes del mismo. -}


-- EJERCICIO 5.2:

{- Estas funciones son polimórficas porque permiten definir una única función para cualquier tipo
   de datos que se invoquen, por ende, no es necesario redefinir la misma función una y otra vez. -}


-- PUNTO 6: Pattern matching sobre listas.


-- EJERCICIO 6.1 (No es un ejercicio):


-- EJERCICIO 6.2:

estaVacia :: [a] -> Bool
-- PRECOND: Ninguna.
estaVacia [] = True
estaVacia _  = False


-- EJERCICIO 6.3:

elPrimero :: [a] -> a
-- PRECOND: La dada lista dada no debe ser vacía.
elPrimero (x:_) = x


-- EJERCICIO 6.4:

sinElPrimero :: [a] -> [a]
-- PRECOND: La dada lista no debe ser vacía.
sinElPrimero (_:xs) = xs


-- EJERCICIO 6.5:

splitHead :: [a] -> (a, [a])
-- PRECOND: La dada lista no debe ser vacía.
splitHead (x:xs) = ((elPrimero (x:xs)), (sinElPrimero (x:xs)))
