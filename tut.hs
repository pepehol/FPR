-- fromIntegral - vezme parametr cele cislo a udela z nej realnce cislo.

-- Guard expressions
-- funkce | podminka
--        | podminka
--        | otherwise

-- klicove slovo "where", za nim se nachazeji lokalni definice, musi mit
-- odsazeni.

-- Ulozeni vice hodnot do jedne promenne - seznamy (list) v Haskellu mame 
-- (jednosmerny) spojove seznamy. Je homogeni datovy typ.
    -- Prazdny seznam = Nil -> []
    -- Pripojovani do seznamu je pomoci : a to vzdy na zacatek seznamu, hlavicku.
    -- Prace se seznamem (x:xs), ukazuje na halvicku seznamu a na zbytek
        -- seznamu.
    -- Pokud chceme vice prvku ze seznamu, muzeme pouzit (x:x1:)
-- Dalsi datova struktura, kterou muzeme pouzit je n-tice.
    -- Muze ukladat ruzne druhy datovych typu.
    -- Ma fixni velikost, nemuze k ni pripojovat dalsi prvky. 
    -- Prace je pomoci zavorek, kdy jednotlive prvky oddelujeme carkou. "(x, y)"
    -- Haskell umoznuje pracovat s n-tici jako s jednim prvkem "x", pro 
        -- vyseparovani muzeme pouzit funkce "fst" a "snd".
        -- "(x:xs) = fst x + snd x"

-- Typova trida Eq zastresuje opreace == a /=
-- Ord = > < <= >=
-- Show zobrazi string v kozoli.
-- Read cte ze vstupu.
-- Enum ciselne typy.
-- Bounded max., min. hodnoty.

-- Integer, dlouhe neomezene cislo.

-- Funkce je hodnota prvniho radu, tzn. funkce je stejne dobra hodnota jako
    -- cokoliv jineho.

-- Vsechny funkce v Haskellu jsou unarni -> curried function.
    -- Unarni funkce vraci mezivysledek. Castecne vyhodnocena funkce.

-- Funkce muze vracet dalsi funkci.
-- Funkce muze pouzivat jako parametr dalsi funkci. (Funkce vyssiho radu. )

-- Konstrukce "let" umoznuje rozdelit kompexni funkci na mensi.
    -- V konstrukci "in" muzu pouzit " "

-- Funkce "words" rozdeli string na slova, oddelena mezerou.

-- Moznost definovat nove operatory, jedna se o vytvoreni "funkce". 
    -- Muzeme nastavit asociativitu.

-- Generatory seznamu. Generuje novy seznam ze seznamu. [x * 2 | x <- [1..10]]
    -- [x * 2 | x <- [1..10], zde muzu zadavat podminky]

-- Generatory pro numericky seznam.
    -- [1..10]
    -- [1..] = nekonecny seznam

-- Pr. pozice Char ve String.
positions :: String -> Char -> [Int]
positions text ch = [position -1 | (letter, position) <- zip text [1..], letter == ch]

-- Lambda vyraz, anonymni funkce. Pouzijeme pokud nechceme vytvaret regulerni
    -- funkci.

-- Pr. Vrat mi ze Stringu slova kratsi/delsi nez.
testString :: [Char]
testString = "ARM je v informatice ozna??en?? architektury procesor?? pou????van??ch d??ky sv?? n??zk?? spot??eb?? elektrick?? energie zejm??na v mobiln??ch za????zen??ch (mobiln?? telefony, tablety), nyn?? v??ak pronik?? i do PC[1]. Glob??ln?? je v roce 2013 ARM nejpo??etn??ji zastoupenou architekturou mikroprocesor??,[2][3][4] p??i??em?? 60 % mobiln??ch za????zen?? na sv??t?? obsahuje ARM ??ip. V roce 2013 bylo vyrobeno 10 miliard ARM procesor??, v roce 2014 u?? 50 miliard.[5] V??voj ARM architektury zapo??al v Brit??nii ve firm?? ARM Holdings v 80. letech 20. stolet??. "

-- filter (\x -> length x < 3) (words testString)

-- Vlastni uzivatelsky datovy typ.
    -- Novy datovy typ pomoci klicoveho slova "data"
        -- data Color = Black | White
-- data Color = Black
--            | White
--            | RGB Int Int Int

-- data Color a = Black
--              | White
--              | RGB a a a

-- Datove typy jako rekurze?

-- data Color a = Black
--              | White
--              | RGB a a a
--              | Mixed [Color a]

-- Aplikovat datove tridy na nas datovy typ.

data Color a = Black
             | White
             | RGB a a a
             | Mixed [Color a] deriving (Show, Eq )

-- isBlack :: Color Int -> Bool 
-- isBlack Black = True 
-- isBlack (RGB x y z) = x == 0 && y == 0 && z == 0
-- isBlack _ = False

-- Omezeni parametru "a"? Nedelame ve vytvorenem datovem typy, ale ve funkcich,
    -- ktere s nima pracuji.

isBlack :: (Num a, Eq a) => Color a -> Bool 
isBlack Black = True 
isBlack (RGB x y z) = x == 0 && y == 0 && z == 0
isBlack _ = False

-- Definovane datove typy "Maybe"

-- Record syntax
data Person = Person {firstName :: String
                      , lastName :: String
                      , age :: Int
                      } deriving (Show)

-- Vestavenne funkce
    -- takeWhile = vrati vse dokud nenarazi na hledany prvek.
        -- takeWhile (/='0') "12 3401234"
        -- "12 34"
    -- dropWhile = vrati vsechny prvky, ktere jsou za hledanym prvkem.
        -- dropWhile (/='0') "12 3401234"
        -- "01234"

data Score = Value Int | Blackjack | Bust deriving (Show, Ord, Eq)
