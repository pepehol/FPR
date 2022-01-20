import Data.List

-- 1.
data Color a = Black
             | White
             | RGB a a a
             | Mixed [Color a] deriving (Show, Eq )

black = Black

rgbTest = Mixed[RGB 155 150 100]

white = White




-- 2.
data FileType = Image | Executable | SourceCode | TextFile deriving (Eq, Show, Read, Ord)
data Entry = File {name :: String, size :: Int, ftype:: FileType}
           | Directory {name :: String, entries :: [Entry]} deriving (Eq, Show, Read, Ord)

root :: Entry
root = Directory "root"
    [
        File "logo.jpg" 5000 Image,
        Directory "classes"
            [
                File "notes-fpr.txt" 200 TextFile,
                File "presentation.jpg" 150 Image,
                File "first_test.jpg" 20 Image
            ]
    ]

-- Napiste funkci, ktera vrati pocet obrazku (FileType Image) v adresarove strukture,
-- predane jako parametr (tedy i ve vsech podadresarich)

countImages :: Entry -> Int
countImages x = test2 "jpg" (test1 x)

-- Vypise obsah "Section".
test1 :: Entry -> [String]
test1 (File n _ _) = [n]
test1 (Directory _ e) = Data.List.concatMap test1 e

test2 :: String -> [String] -> Int
test2 _ [] = 0
test2 y (x:xs) = if (y `isPrefixOf`) `findIndex` (tails x) > Just 0 then 1 + test2 y xs else test2 y xs





