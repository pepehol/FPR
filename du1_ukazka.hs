import Data.Char ( ord )
-- Funkce "ord" prevede znaky na ascii hodnotu.
-- pp(ships sampleInput [('a',1),('d',1),('d',2),('c',1),('b',1),('e',1),('f',1),('g',1),('c',7),('c',10)])
-- show prevede cislo na string
type Result = [String]

pp :: Result -> IO ()
pp x = putStr (concat (map (++"\n") x))

sampleInput :: Result
sampleInput = ["  o    o  ",
               "      ooo ",
               "   oo     ",
               "          ",
               "     o    ",
               "     o    ",
               "     o    ",
               "          ",
               "          ",
               "  oooo    "]

-- ships :: Result -> [(Char, Int)] -> Result
ships input coordinates = let
    coordinates' = [(ord ch - ord 'a' + 1, ri) | (ch, ri) <- coordinates]
    get x ch | elem x coordinates' = if ch == 'o' then 'x' else '.'
             | otherwise = ch
    niceShow x = let 
        number' = show x
        in if length number' == 1 then "  " ++ number' else " " ++ number'
    nicePrint result = reverse [niceShow number ++ row |  (number, row) <- zip[1..] result] ++ ["   abcdefghij"]
    in nicePrint ([[get (ci, ri) ch | (ci, ch) <- zip [1..] row] | (ri, row) <- zip [1..] (reverse input)])