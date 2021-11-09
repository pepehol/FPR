{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- Spocitej delku listu.
length' :: [a] -> Int
length' [] = 0
length'(_:xs) = 1 + length' xs

-- Udelej sumu prvku Int v listu.
sumIt :: [Int] -> Int
sumIt []  = 0
sumIt (x : xs) = x + sumIt xs

-- Ziskej prvni prvek z listu.
getHead :: [a] -> a
getHead (x : _) = x

-- Ziskej posledni prvek z listu.
getLast :: [a] -> a
getLast [x] = x
getLast (_ : xs) = getLast xs

-- Zjisti zda je prvek prvkem listu.
isElement :: Eq a => a -> [a] -> Bool
isElement _ [] = False
isElement a (x : xs) | a == x = True
                     | otherwise = isElement a xs

-- Ziskej list bez prvniho prvku.
getTail :: [a] -> [a]
getTail (_ : xs) = xs

-- Ziskej list bez posledniho posledniho prvku.
getInit :: [a] -> [a]
getInit [x] = []
getInit (x : xs) = x : getInit xs

-- Napis funkci, ktera spoji dva seznamy do jednoho.
combine :: [a] -> [a] -> [a]
combine [] y = y
combine (x : xs) y = x : combine xs y

-- Napis funkci ktera vybere element s nejvyssi hodnotou.
max' :: [Int] -> Int
max' (x : xs) = tmp x xs where
    tmp m [] = m
    tmp m (x : xs) | m < x = tmp x xs
                   | otherwise = tmp m xs

max'' :: [Int] -> Int
max'' [x] = x
max'' (x : y : rest) | x < y = max'' (y : rest)
                     | otherwise = max'' (x : rest)

-- Napis funkci ktera prehaze prvky v seznamu.
reverse' :: [a] -> [a]
reverse' [] = []
reverse'(x : xs) = reverse' xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' x = tmp x [] where
    tmp [] ys = ys 
    tmp (x : xs) ys = tmp xs (x : ys)

-- Napiste funkcinasobeni skalarem
scalar :: [Int] -> [Int] -> Int
scalar [] [] = 0
scalar (x : xs) (y : ys) = x * y + scalar xs ys