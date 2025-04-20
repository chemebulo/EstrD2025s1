module Queue
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
where

data Queue a = Q []
    deriving Show
{- INV. REP.:
    * Sea Q []: .
    * Sea Q []: . 
-}

{- COSTO OPERACIONAL DE CADA FUNCIÓN:

- emptyQ        O()
- isEmptyQ      O()
- enqueue       O()
- firstQ        O()
- dequeue       O()

-}

