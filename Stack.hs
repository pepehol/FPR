module Stack (Stack, push, pop, top, isEmpty, empty) where

newtype Stack a = Stack [a] deriving Show

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> Stack a
pop (Stack (_:xs)) = Stack xs

top :: Stack a -> a
top (Stack (x:_)) = x 

isEmpty :: Stack a ->Bool
isEmpty (Stack []) = True
isEmpty _ = False

empty :: Stack a
empty = Stack []