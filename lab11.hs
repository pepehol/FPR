-- Datovy typ strom. Binarniho typu. Hodnoty ulozene v listech a ve vetvich.
data Tree a = Leaf a 
            | Branch a (Tree a) (Tree a) deriving (Show)

-- Priklady binarniho stromu.
testTree1 :: Tree Int 
testTree1 = Branch 12 (Branch 23 (Leaf 34) (Leaf 45)) (Leaf 55)

testTree2 :: Tree Char
testTree2 = Branch 'a' (Branch 'b' (Leaf 'c') (Leaf 'd')) (Leaf 'e')

-- Funkce spocita vsechny prvky ve strome.
sum' :: Tree Int -> Int
sum' (Leaf x) = x
sum' (Branch x l r) = sum' l + x + sum' r

-- Funkce prevede hodnoty ze stromu do seznamu.
toList :: Tree a -> [a]
toList (Leaf x) = [x]
toList (Branch x l r) = toList l ++ [x] ++ toList r


-- Binarni strom do textove a z textove formy.
-- a(b(d,e),c(e,f(g,h)))

toString :: Show a => Tree a -> String
toString (Leaf x) = show x
toString (Branch x lt rt) = show x ++ "(" ++ toString lt ++ "," ++ toString rt ++ ")"

fromString :: Read a => String -> Tree a
fromString inp = fst (fromString' inp) where
    fromString' :: Read a => String -> (Tree a, String)
    fromString' inp = let
        before = takeWhile (\x -> x /= '(' && x /= ',' && x /= ')') inp
        after = dropWhile (\x -> x /= '(' && x /= ',' && x /= ')') inp
        value = read before 
        in if null after || head after /= '(' then (Leaf value, after)
           else let
               (l, after') = fromString' (tail after)
               (r, after'') = fromString' (tail after')
               in (Branch value l r, tail after'')
