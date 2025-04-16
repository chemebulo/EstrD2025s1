module SetV1
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
where

data Set a = TRS [a] Int
                     -- Cantidad de Elementos en [a]
    deriving Show
    {- INV. REP.:
        * Sea TRS xs n, no hay elementos repetidos en xs.
        * 
    -}


emptyS :: Set a
-- PROP: Crea un conjunto vacío.
emptyS = TRS [] 0


addS :: Eq a => a -> Set a -> Set a
-- PROP: Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS x (TRS xs n) = if perteneceA x xs
                       then TRS xs n
                       else TRS (x:xs) (n+1)

perteneceA :: Eq a => a -> [a] -> Bool
perteneceA x []     = False
perteneceA x (y:ys) = x == y || perteneceA x ys


belongs :: Eq a => a -> Set a -> Bool
-- PROP: Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs x (TRS xs) = perteneceA x xs 


sizeS :: Eq a => Set a -> Int
-- PROP: Devuelve la cantidad de elementos distintos de un conjunto.
sizeS (TRS xs) = length xs


removeS :: Eq a => a -> Set a -> Set a
-- PROP: Borra un elemento del conjunto.
removeS x (TRS xs) = if perteneceA x xs
                        then TRS (removeX x xs)
                        else TRS xs

removeX :: Eq a => a -> [a] -> [a]
removeX x []     = []
removeX x (y:ys) = if x==y
                      then ys
                      else removeX x ys 


unionS :: Eq a => Set a -> Set a -> Set a
-- PROP: Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos.
unionS (TRS xs) (TRS ys) = TRS (unionDe xs ys)

unionDe :: Eq a => [a] -> [a] -> [a]
-- PRECOND: Cada lista no contiene elementos repetidos, pero puede llegar a coincidir que ambas tengan el mismo elemento.
unionDe []     ys     = ys
unionDe xs     []     = xs
unionDe (x:xs) (y:ys) = if not (perteneceA x (unionDe xs ys))
                           then x : unionDe xs ys
                           else unionDe xs ys


setToList :: Eq a => Set a -> [a]
-- PROP: Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList (TRS xs) = xs
