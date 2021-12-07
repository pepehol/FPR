module Queue (Queue, addQ, remQ, isEmptyQ, emptyQ) where

data Queue a = Qu [a] deriving Show

emptyQ :: Queue a
emptyQ = Qu []

isEmptyQ :: Queue a -> Bool 
isEmptyQ (Qu q) = null q

addQ :: a -> Queue a -> Queue a
addQ x (Qu xs) = Qu (xs++[x])

remQ :: Queue a -> (a, Queue a)
remQ q@(Qu xs) | not (isEmptyQ q) = (head xs, Qu(tail xs))
               | otherwise = error "remQ"