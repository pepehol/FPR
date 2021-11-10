import Data.Char
-- Vygeneruj seznam vsech lichych prvku v zadanem intervalu.
-- Zde pomoci generatoru seznamu.
oddList :: Int -> Int -> [Int]
oddList a b = [x | x <- [a..b], odd x]

-- Funkce smaze vsechny prvky, kde se vyskytuji velka pismena.
removeAllUpper :: String -> String
removeAllUpper xs = [x | x <- xs, not (isUpper x)]

-- Funkce provede sjednoceni a prunik.
union :: Eq a => [a] -> [a] -> [a]
union xs ys = xs ++ [y | y <- ys, not (elem y xs)]

insersection :: Eq a => [a] -> [a] -> [a]
insersection xs ys = [y | y <- ys, elem y xs]

-- Funkce spocita pocet jednotlivych znaku.
countThem :: String -> [(Char, Int)]
countThem xs = let u = unique xs
               in [(x, length (filter( == x) xs)) | x <- u]

unique :: String -> String
unique [] = []
unique (x : xs) = x : unique (filter (/=x) xs)

-- Goldbachova hypoteza. Kazde sude cislo vetsi nez dve je mozne ziskat
-- sectenim dvou prvocisel.
isPrime :: Int -> Bool
isPrime x = null [y |y <- [2..ceiling(sqrt(fromIntegral (x - 1)))], x `mod` y == 0]

goldbach :: Int -> [(Int, Int)]
goldbach n = let primes = [x |x <- [2..(n `div` 2) + 1], isPrime x]
             in [(x, n - x) | x <- primes, isPrime(n - x)]

-- Goldbachova hypoteza. Funkce ma tri parametry. Prvni dva urcuji interval
-- kde se bude hledat Goldbachova hypoteza. Treti urcuje limitu, kdy z 
-- nalezenych prvocisel musi byt to mensi, prave vetsi nez dana limita.
goldbachList :: Int -> Int -> Int -> [(Int, Int)]
goldbachList a b limit = filter (\(x, _) -> x > limit) [head (goldbach x) | x <- [a..b], even x]

-- Funkce vygeneruje vsechny kombinace daneho vstupniho retezce, dalsi
--  parametr omezi velikost dane kombinace.
combinations :: Int -> String -> [String]
combinations 1 xs = [[x] | x<-xs]
combinations n (x : xs) | n == length (x:xs) = [(x:xs)]
                        |otherwise = [x: y |y<-combinations (n-1) xs] ++ combinations n xs

-- Kontrola zda se nam vse povedlo.
-- FAKTORIAL
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- KOMBINACE
combinations' :: Int -> Int -> Int
combinations' k n = fromIntegral(factorial n) `div` fromIntegral(factorial k * factorial (n-k))