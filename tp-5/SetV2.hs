module SetV2
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
where

data Set a = TRS [a] Int
    deriving Show
{- INV. REP.:
    * Sea (TRS xs n): puede haber elementos repetidos en xs.
    * Sea (TRS xs n): n equivale a la cantidad de elementos en xs.
-}

{- COSTO OPERACIONAL DE CADA FUNCIÓN:

- emptyS        O(1)
- addS          O(1)
- belongs       O(n)
- sizeS         O(n^2)
- removeS       O(n^2)
- unionS        O(1)
- setToList     O(n)

-}


emptyS :: Set a
-- PROP: Crea un conjunto vacío.
emptyS = TRS [] 0


addS :: Eq a => a -> Set a -> Set a
-- PROP: Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS x (TRS xs n) = TRS (x:xs) (n+1)


belongs :: Eq a => a -> Set a -> Bool
-- PROP: Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs x (TRS xs n) = perteneceA x xs 

perteneceA :: Eq a => a -> [a] -> Bool
perteneceA x []     = False
perteneceA x (y:ys) = x == y || perteneceA x ys


sizeS :: Eq a => Set a -> Int
-- PROP: Devuelve la cantidad de elementos distintos de un conjunto.
sizeS (TRS xs n) = length (sinRepes xs)

sinRepes :: Eq a => [a] -> [a]
sinRepes []     = []
sinRepes (x:xs) = if perteneceA x xs
                     then sinRepes xs
                     else x : sinRepes xs


removeS :: Eq a => a -> Set a -> Set a
-- PROP: Borra un elemento del conjunto.
removeS x (TRS xs n) = TRS (removeX x xs) (length (removeX x xs))

removeX :: Eq a => a -> [a] -> [a]
removeX x []     = []
removeX x (y:ys) = if x==y
                      then ys
                      else y : removeX x ys 


unionS :: Eq a => Set a -> Set a -> Set a
-- PROP: Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos.
unionS (TRS xs n1) (TRS ys n2) = TRS (xs ++ ys) (n1 + n2)


setToList :: Eq a => Set a -> [a]
-- PROP: Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList (TRS xs n) = sinRepes xs