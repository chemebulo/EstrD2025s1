-- PUNTO 2: Números Enteros. 

-- EJERCICIO 1, A:

sucesor :: Int -> Int
-- PRECOND: Ninguna.
sucesor x = x+1


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