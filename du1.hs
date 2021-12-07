-- 2 - Crossword Answers

-- Implement the function answers that takes as an input a crosword puzzle's 
-- solution and outputs all words from this solution. Words will be divided 
-- into two lists, first for lines (from left to right) and second for columns 
-- (from top to bottom). A word is written only if it is longer than just one 
-- character.

--     TIP: Use the function words :: String -> [String] to split lines into 
-- sequence of words.

-- *Main> words "ABC cdef ghijkl"
-- ["ABC","cdef","ghijkl"]

type Result = [String]
solution1 :: Result
solution1 = ["DAD  SEND",
             "O EAST  A",
             "W A  ITSY",
             "NERF N T ",
             " A ARK U ",
             " S T SYNC",
             "MESH  A A",
             "A  EVER R",
             "NEAR  D D"]

-- answers :: Result -> ([String],[String])

-- *Main> answers solution1 
-- (["DAD","SEND","EAST","ITSY","NERF","ARK","SYNC","MESH","EVER","NEAR"],
-- ["DOWN","MAN","EASE","DEAR","FATHER","STINKS","YARD","STUN","DAY","CARD"])


answers :: Result -> ([String],[String])
answers solution = let
    -- Funkce pro otoceni seznamu.
    rotate :: Result -> Result
    rotate x = foldl1 sideBySide (map (toRow) x) where
        toRow :: String -> Result
        toRow row = [[x] | x<- row]
        sideBySide :: Result -> Result -> Result
        sideBySide xs ys = map (\(x, y) -> x ++ y)(zip xs ys)

    -- Funkce pro zanechani itemu v seznamu, ktere jsou delsi nez 2 znaky.
    separeX :: Result -> Result
    separeX x  = [ separe y | y <- x] where
        separe x = if length x > 2 then x else ""

    joinS' :: Result -> [Result]
    joinS' x = [ words y | y <- x]

    removeItem :: String  -> Result -> Result
    removeItem _ [] = []
    removeItem x (y:ys) | x == y = removeItem x ys
                        | otherwise = y : removeItem x ys
    in (removeItem "" (separeX (concat (joinS' solution))), 
    removeItem "" (separeX (concat (joinS' (rotate solution)))))


-- Doplnkova uloha
answers1 :: Result -> Int
answers1 solution = let
    -- Funkce pro zanechani itemu v seznamu, ktere jsou delsi nez 2 znaky.

    joinS' :: Result -> [Result]
    joinS' x = [ words y | y <- x]

    vowels []  = 0
    vowels (x : xs) = if x == ' ' then 1 + vowels xs else vowels xs
    in vowels (concat solution)

searchChar :: Char
searchChar = 'A'
-- Doplnkova uloha
countChars :: Result -> Char-> Int
countChars [] s = 0
countChars(x:xs) s = count x where
        count [] = countChars xs s
        count (y:ys) = if y == s then 1 + count ys else count ys

-- Spocitej delku listu.
length' :: [a] -> Int
length' [] = 0
length'(_:xs) = 1 + length' xs

-- POMOCNE PRIKLADY ////////////////////////////////////////////////////////////

-- Otoceni seznamu.
rotate :: Result -> Result
rotate x = foldl1 sideBySide (map (toRow) x) where
    toRow :: String -> Result
    toRow row = [[x] | x<- row]

    sideBySide :: Result -> Result -> Result
    sideBySide xs ys = map (\(x, y) -> x ++ y)(zip xs ys)



-- Pro vyseparovani retezcu ze Stringu.
separeX :: Result -> Result
separeX x  = [ separe y | y <- x] where
    separe x = if length x > 2 then x else ""

joinS' :: Result -> [Result]
joinS' x = [ words y | y <- x]

removeItem :: String  -> Result -> Result
removeItem _ [] = []
removeItem x (y:ys) | x == y = removeItem x ys
                    | otherwise = y : removeItem x ys

-- Spojeni dvou seznamu.
joinTwoL :: Result -> Result -> (Result, Result)
joinTwoL x y = (x , y)

-- Ukazkovy vstup joinTwoL (removeItem "" (separeX (concat (joinS' solution1))))
--  (removeItem "" (separeX (concat (joinS' (rotate solution1)))))