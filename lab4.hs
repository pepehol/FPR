import Data.Char
-- Funkce vezme n prvku ze seznamu.
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

-- Funkce ktera vynecha n prvku a vrati zbyly seznam.
drop' :: Int -> [a] -> [a]
drop' 0 x = x
drop' _ [] = []
drop' n (_ : xs) = drop' (n - 1) xs

-- Nalezni minimum v seznamu.
minimum' :: Ord a => [a] -> a
minimum' [x] = x
minimum' (x : y : z) | x < y = minimum' (x : z)
                     | x > y = minimum' (y : z)

-- Najdi vsechny delitele vstupniho cisla.
divisors :: Int -> [Int]
divisors n = reverse(tmp n) where
    tmp 0 = []
    tmp i | n `mod` i == 0 = i : tmp (i - 1)
          | otherwise = tmp (i - 1)

-- Pomoci generatoru seznamu.
divisors' :: Int -> [Int]
divisors' n = [i | i <- [1..n], n `mod` i == 0]

-- Pomoci filtru
divisors'' :: Int -> [Int]
divisors'' n = filter (\x -> n `mod` x == 0) [1..n]

-- Funkce spoji dva seznamy do jedne n-tice.
zipThem :: [a] -> [b] -> [(a,b)]
zipThem (x:xs) (y:ys) = (x,y) : zipThem xs ys
zipThem _ _ = []

-- Funkce provede soucin dvou vektoru.
dotProduct :: [a] -> [b] -> [(a, b)]
dotProduct [] _ = []
dotProduct (x : xs) ys = tmp ys ++ dotProduct xs ys where
    tmp [] = []
    tmp (b : bs) = (x, b) : tmp bs

dotProduct' :: [a] -> [b] -> [(a, b)]
dotProduct' xs ys = [(x, y) | x <- xs, y <- ys]

dotProduct'' :: [a] -> [b] -> [(a, b)]
dotProduct'' x y =
    zip (concat (map (replicate (length y)) x))
                        (concat (replicate (length x) y))

-- Vypocet Fibonacciho posloupnosti s pomoci n-tic.
fibonacci :: Int -> Int
fibonacci n = fst (tmp n) where
    fibStep (u, v) = (v, u + v)
    tmp 0 = (0, 1)
    tmp n = fibStep (tmp (n - 1))

-- High-order funkce.
-- Funkce vezme vstupni parametr typu string a prevede vsechny znaky na velke
-- pismena
allToUpper :: String -> String
allToUpper xs = [toUpper x | x <- xs]

allToUpper' :: String -> String
allToUpper' xs = map toUpper xs

-- Vytvorte vlastni algoritmus quick sort, pivota vybirejte vzdy prvni prvek.
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = let lp = filter (< x) xs
                       rp = filter (>= x) xs
                   in quicksort lp ++ [x] ++ quicksort rp