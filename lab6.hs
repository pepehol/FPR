import Data.List (sortBy)

-- Funkce ktere operuji s logickymi operatorami.
-- not
-- and
-- or
-- nand
-- xor
-- impl
-- equ
not' :: Bool -> Bool
not' True = False 
not' False = True
infixl 5 `not'`

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False
infixl 4 `and'`

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True
infixl 3 `or'`

nand' :: Bool -> Bool -> Bool
nand' x y = not' (and' x y)
infixl 4 `nand'`

-- /= oposite ==
xor' :: Bool -> Bool -> Bool
xor' x y = x /= y
infixl 3 `xor'`

impl' :: Bool -> Bool -> Bool
impl' True False = False
impl' _ _ = True
infixl 2 `impl'`

equ' :: Bool -> Bool -> Bool
equ' x y = x == y
infixl 7 `equ'`

table :: (Bool -> Bool -> Bool) -> IO ()
table expr = putStr (concat [nicePrint [x, y, (expr x y)] |x <- [True, False], y <- [True, False]])

nicePrint :: [Bool] -> String
nicePrint xs = concat [show x ++ "\t" | x <- xs] ++ "\n"

-- Uprav Funkci "table". Pridej parametr, ktery bude udavat pocet promennych.
tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = putStr(concat [nicePrint x ++ " => " ++ show(f x) ++ "\n" | x <- allValues n]) where
    allValues 1 = [[True], [False]]
    allValues n = [x : y | x <- [True, False], y <- allValues (n - 1)]

    nicePrint :: [Bool] -> String
    nicePrint xs = concat [show x ++ "\t" | x <- xs]

-- Funkce pro pocet Huffmanova kodu.
huffman :: [(Char, Int)] -> [(Char, String)]
huffman input = let prepare = [(y, [(x, "")]) | (x, y) <- input]
                in step prepare where
    step :: [(Int, [(Char, String)])] -> [(Char, String)]
    step [(_, result)] = result
    step tree = let ((a1, a2) : (b1, b2) : rest) = sortBy (\(x, _) (y, _) -> compare x y) tree
                in step ((a1 + b1, [(x, '0' : y) | (x, y) <- a2 ] ++ [(x, '1' : y) | (x, y) <- b2 ]) : rest)