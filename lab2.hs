-- 5 + 8             prosty soucet
-- 3 * 5 + 8         prednost operatoru, viz. prezentace
-- 2 + 4             prosty soucet
-- sqrt 16           odmocnina
-- succ 6            incrementace 
-- succ 7            incrementace
-- pred 9            dekrementace
-- pred 8            dekrementace
-- sin (pi / 2)      vypocet sinusu
-- truncate pi       zaokrouhleni dolu
-- round 3.5         zaokrouhleni s rozhodovacim bodem 0,5
-- round 3.4         zaokrouhleni s rozhodovacim bodem 0,5
-- floor 3.7         zaokrouhleni dolu
-- ceiling 3.3       zaokrouhleni nahoru
-- mod 10 3          zbytek po celociselnem deleni
-- odd 3             vraci True pokud je cislo liche, pokud sude vraci False


-- FAKTORIAL
-- prvni moznost
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- druha moznost
factorial' :: Int -> Int
factorial' n | n == 0 = 1
             | otherwise = n * factorial' (n - 1)
-- treti moznost
factorial'' :: Int -> Int
factorial'' n = if n==0 then 1 else n * factorial'' (n - 1)


-- FIBONACIHO CISLO
-- 1. moznost
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- 2. moznost
fib' :: Int -> Int
fib' n | n==0 = 0
       | n==1 = 1
       | otherwise = fib' (n - 1) + fib' (n - 2)

-- 3. moznost 
fib'' :: Int -> Int
fib'' n = tmp n 1 1 where
       tmp 0 a _ = a
       tmp x a b = tmp (x - 1) b (a + b)


-- PRESTUPNY ROK
-- 1. moznost
leapYear :: Int -> Bool
leapYear x = x `mod` 4 == 0 && x `mod` 100 /= 0 || x `mod` 400 == 0

-- 2. moznost
leapYear' :: Int -> Bool
leapYear' x | x `mod` 400 == 0 = True
            | x `mod` 100 == 0 = False
            | otherwise = x `mod` 4 == 0


-- VRAT MAXIMUM
max2 :: Int -> Int -> Int
max2 a b = if a >= b then a else b

max3 :: Int -> Int -> Int -> Int
max3 x y z = (x `max2` y) `max2` z

-- KOMBINACE
combinations :: Int -> Int -> Int
combinations n k = factorial n `div` (factorial k * factorial (n-k))

combinations' :: Int -> Int -> Int
combinations' n k = fromIntegral(factorial n) `div` fromIntegral(factorial k * factorial (n-k))

-- NAJDI KORENY FUNKCE
numberOfRoots :: Int -> Int -> Int -> Int
numberOfRoots a b c = let d = b^2 - 4 * a * c
                      in if d > 0 then 2 else if d == 0 then 1 else 0

-- NAJDI NEJVETSIHO SPOLECNEHO DELITELE
gcd' :: Int -> Int -> Int 
gcd' a b | a == b = a
         | a < b = gcd' a (b - a)
         | otherwise = gcd' (a -b) b

gcd2 :: Int -> Int -> Int 
gcd2 a 0 = a
gcd2 a b = gcd2 b (a `mod` b)

isPrime :: Int -> Bool
isPrime 1 = True
isPrime n = isPrimeTest n (n - 1) where
       isPrimeTest n 1 = True
       isPrimeTest n i | n `mod` i == 0 = False
                     | otherwise = isPrimeTest n (i - 1)