reverse' :: [a] -> [a]
reverse' [] = []
reverse'(x : xs) = reverse' xs ++ [x]

-- Typove synonymum pro seznam stringu.
type Pic = [String]
pic :: Pic
pic = [ "....#....",
        "...###...",
        "..#.#.#..",
        ".#..#..#.",
        "....#....",
        "....#....",
        "....#####"]

-- Funkce prijma typ Pic a vytiskne jej do konzole.
pp :: Pic -> IO ()
pp x = putStr (concat (map (++"\n") x))

-- Funkce vertikalne obraci seznam typu Pic.
flipV :: Pic -> Pic
flipV = map reverse'

flipV' :: Pic -> Pic
flipV' xs = [reverse' x | x <- xs]

-- Funkce horizontalne obraci seznam typu Pic.
flipH :: Pic -> Pic
flipH x = reverse' x

-- Funkce vytiskne dva seznamy pod sebou.
above :: Pic -> Pic -> Pic
above x y = x ++ y

-- Funke vytiskne dva seznamy vedle sebe.
-- zip = spoji dva seznamy do jednoho seznamu.
-- zip :: [a] -> [b] -> [(a, b)]   -- Defined in ‘GHC.List’
-- map = provede operaci na druhem seznamu, podle prvniho parametru.
sideBySide :: Pic -> Pic -> Pic
sideBySide xs ys = map (\(x, y) -> x ++ y)(zip xs ys)

sideBySide' :: Pic -> Pic -> Pic
sideBySide' (x : xs) (y : ys) = (x ++ y) : sideBySide' xs ys
sideBySide' _ _  = []

sideBySide'' :: Pic -> Pic -> Pic
sideBySide'' = zipWith (++)

-- Funkce rotovani sipky do prava a do leva.
rotateR :: Pic -> Pic
rotateR [x] = toRow x
rotateR (x : xs) = rotateR xs `sideBySide` (toRow x)

toRow :: String -> Pic
toRow row = [[x] | x<- row]

rotateL :: Pic -> Pic
rotateL [x] = reverse (toRow x)
rotateL (x : xs) = reverse (toRow x) `sideBySide` (rotateL xs)

zoom :: Int -> Pic -> Pic
zoom n xs = [concat(map (replicate n) x) | x <- concat (map (replicate n) xs)]