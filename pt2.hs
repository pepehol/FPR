-- 1.
-- Nadefinujte rekurzivni datovou strukturu Company, ktera bude obsahovat
-- pojmenovane polozky:
    -- name - nazev firmy typu String
    -- employees - pocet zamestnancu (Int)
    -- ownerOf - seznam dalsich firem, ktere jsou firmou vlastneny.

data HTMLDocument = Tag { nameType :: String,
                          attributes :: Attribute,
                          tags :: String
                        } deriving (Show)

data Attribute = Attribute { name :: String,
                            value :: String
                            } deriving(Show)

-- 2.
data Article = Text String
             | Section String [Article] deriving (Eq, Show, Read, Ord)

myArticle :: Article
myArticle = Section "Document" [
                Section "Introduction" [
                    Text "My introduction",
                    Section "Notation" [Text "alpha beta gamma"]
                ],

                Section "Methods" [
                    Section "Functional Programming" [Text "FPR"],
                    Section "Logical Programming" [Text "LPR"]
                ],

                Section "Result" [Text "All is great"]
            ]

myArticle1 :: Article
myArticle1 = Text "Ahoj"


-- Implementujte funkci, ktera vrati pocet bloku textu (Text) v clanku predaneho
-- jako parametr.

-- Spocti pocet vyskytu "Section".
sectionCount :: Article -> Int
sectionCount a = length' (sectionContent a)

-- Spocti pocet vyskytu "Text".
textCount :: Article -> Int
textCount a = length' (textContent a)

-- Vypise obsah "Text".
textContent :: Article -> [String]
textContent (Text x) = [x]
textContent (Section _ t) = concatMap textContent t

-- Vypise obsah "Section".
sectionContent :: Article -> [String]
sectionContent (Text _) = []
sectionContent (Section s a) = s : concatMap sectionContent a 

-- Vypise obsah "Section" a "Text".
allContent :: Article -> [String]
allContent (Text x) = [x]
allContent (Section s a) = s : concatMap allContent a

-- Spocti prvky v listu.
length' :: [a] -> Int
length' [] = 0
length'(_:xs) = 1 + length' xs

-- 3.
-- Implementujte funkci, ktera vrati seznam jmen kapitol, ktere obsahuji ve svem
-- seznamu typu [Article] bloky textu.

test3 :: Article -> Int
test3 (Text _) = 0
test3 (Section _ _) = 1

test2 :: [Article] -> Article
-- test2 [] = []
test2 (x : _) = test4 x

test1 :: Article -> Article
-- test1 (Text x) = 
test1 (Section _ a) = test2 a


test4 :: Article -> Article
test4 x = x



-- Vypise obsah "Section".
test5 :: Article -> [Int]
test5 (Text _) = [0]
test5 (Section s a) = 1 + concatMap test5 a 
