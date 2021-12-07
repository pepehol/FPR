-- 4 - Poker

-- Lets define a data types representing a deck of Poker cards. In Poker, a 
-- player gets 5 cards into the hand. Dealt hands are classified into several
-- categories. These categories are important to define who is the winner.
-- Rules for each category cen be found Here

-- Write a function decide that takes 5 dealt cards - Hand and returns a poker category in which it fits.

-- decide:: Hand -> Category

-- Prelude> decide [Card (Numeric 2) Hearts,Card (Numeric 2) Clubs,Card Ace Hearts,Card Ace Clubs,Card King Spades]
-- TwoPair
-- Prelude> decide [Card (Numeric 2) Hearts,Card (Numeric 2) Clubs,Card Ace Hearts,Card Ace Clubs,Card Ace Spades]
-- FullHouse
-- Prelude> decide [Card Ace Hearts,Card (Numeric 2) Hearts,Card (Numeric 5) Hearts,Card (Numeric 3) Hearts,Card (Numeric 4) Clubs]
-- Straight
-- Prelude> decide [Card (Numeric 2) Hearts,Card (Numeric 5) Clubs,Card Ace Hearts,Card King Clubs,Card Jack Spades]
-- HighCard

data Suit = Hearts | Clubs | Diamonds | Spades deriving (Eq, Show)
data Rank = Numeric Int | Jack | Queen | King | Ace deriving (Eq, Show, Ord)
data Card = Card Rank Suit deriving (Eq, Show)
type Hand = [Card]
data Category = RoyalFlush
              | StraightFlush
              | Four
              | FullHouse
              | Flush
              | Straight
              | Three
              | TwoPair
              | Pair
              | HighCard deriving (Eq, Show)

-- TwoPair
test1 :: [Card]
test1 = [Card (Numeric 2) Hearts,Card (Numeric 2) Clubs,Card Ace Hearts,Card Ace Clubs,Card King Spades]

-- FullHouse
test2 :: [Card]
test2 = [Card (Numeric 2) Hearts,Card (Numeric 2) Clubs,Card Ace Hearts,Card Ace Clubs,Card Ace Spades]

-- Straight
test3 :: [Card]
test3 = [Card Ace Hearts,Card (Numeric 2) Hearts,Card (Numeric 5) Hearts,Card (Numeric 3) Hearts,Card (Numeric 4) Clubs]

-- HighCard
test4 :: [Card]
test4 = [Card (Numeric 2) Hearts,Card (Numeric 5) Clubs,Card Ace Hearts,Card King Clubs,Card Jack Spades]

-- Flush
test5 :: [Card]
test5 = [Card (Numeric 2) Hearts,Card (Numeric 5) Hearts,Card Ace Hearts,Card King Hearts,Card Jack Hearts]

-- 
test6 :: [Card]
test6 = [Card (Numeric 2) Hearts,Card (Numeric 5) Hearts,Card (Numeric 3) Hearts,Card (Numeric 10) Hearts,Card (Numeric 7) Hearts]

-- Royal Flush
test7 :: [Card]
test7 = [Card Ace Hearts,Card King Hearts,Card Queen Hearts,Card (Numeric 10) Hearts,Card Jack Hearts]

-- Ziskej hodnotu karet.
getRanks :: Hand -> [Rank]
getRanks [] = []
getRanks ((Card x _) : xs) = x : getRanks xs

-- Ziskej barvu karet.
getSuits :: Hand -> [Suit]
getSuits [] = []
getSuits ((Card _ x) : xs) = x : getSuits xs

-- Spocti pocet vyskytu stejne karty.
countRanks :: [Rank] -> [(Rank, Int)]
countRanks xs = let u = unique xs
               in [(x, length (filter( == x) xs)) | x <- u] where
                    unique :: [Rank] -> [Rank]
                    unique [] = []
                    unique (x : xs) = x : unique (filter (/=x) xs)

-- Setrizeni podle hodnoty.
sortByRank :: [Rank] -> [Rank]
sortByRank [] = []
sortByRank (x:xs) = let lp = filter (< x) xs
                        rp = filter (>= x) xs
                    in sortByRank lp ++ [x] ++ sortByRank rp

-- Vyskytuje se v ruce konkretni pocet?
     -- 1 - samostatna karta
     -- 2 - par atd.
isConcreteRank :: Int -> [(Rank, Int)] -> Int
isConcreteRank _ [] =  0
isConcreteRank n ((_, x) : xs) = if n == x then 1 + isConcreteRank n xs else isConcreteRank n xs

-- Vyskytuje se pocet vyskytu?
numberOfOccurrences :: Int -> [(Rank, Int)] -> Bool
numberOfOccurrences _ [] = False
numberOfOccurrences n ((_, x) : xs) = (x >= n) || numberOfOccurrences n xs

-- Mam na ruce jednu barvu?
isSameSuit :: [Suit] -> Bool
isSameSuit [] = True
isSameSuit [x] = True
isSameSuit (x:x1:xs) = x == x1 && isSameSuit (x1:xs)

-- Vygeneruj mi postupku
generateStraight :: Rank -> [Rank]
generateStraight (Numeric x) | x == 7 = [Numeric n | n <- [x..10]] ++ [Jack]
                             | x == 8 = [Numeric n | n <- [x..10]] ++ [Jack, Queen]
                             | x == 9 = [Numeric n | n <- [x..10]] ++ [Jack, Queen, King]
                             | x == 10 = [Numeric n | n <- [x..10]] ++ [Jack, Queen, King, Ace]
                             | otherwise = [Numeric r | r <- take 5 [x..]]
generateStraight Ace = [Numeric r | r <- take 4 [2..]] ++ [Ace]
generateStraight _ = []

-- Pomoci konstrukci "if".
-- decide:: Hand -> Category
-- decide hand = if numberOfOccurrences 2 (countRanks (getRanks hand)) then
--                     if numberOfOccurrences 4 (countRanks (getRanks hand)) then
--                          Four
--                     else
--                          if numberOfOccurrences 3 (countRanks (getRanks hand)) then
--                               if numberOfOccurrences 2 (countRanks (getRanks hand)) then
--                                    FullHouse
--                               else
--                                    Three
--                          else
--                               if isConcreteRank 2 (countRanks (getRanks test1)) >= 2 then
--                                    TwoPair
--                               else
--                                    Pair
--                else
--                     if isSameSuit (getSuits hand) then
--                          if sortByRank (getRanks hand) == generateStraight rank then
--                               if rank == Numeric 10 then
--                                    RoyalFlush
--                               else
--                                    StraightFlush
--                          else
--                               Flush
--                     else
--                          if sortByRank (getRanks hand) == generateStraight rank then 
--                               Straight
--                          else
--                               HighCard
--      where
--           rank = if head (sortByRank (getRanks hand)) == Numeric 2 && last (sortByRank (getRanks hand)) == Ace then
--                Ace
--           else
--                head (sortByRank (getRanks hand))

-- Pomoci konstrukci Guard expressions.
decide:: Hand -> Category
decide hand | numberOfOccurrences 2 (countRanks (getRanks hand)) =
               if numberOfOccurrences 4 (countRanks (getRanks hand))
               then
                    Four
               else
                    if numberOfOccurrences 3 (countRanks (getRanks hand))
                    then
                         if numberOfOccurrences 2 (countRanks (getRanks hand))
                         then
                              FullHouse
                         else
                              Three
                    else
                         if isConcreteRank 2 (countRanks (getRanks test1)) >= 2
                         then
                              TwoPair
                         else
                              Pair
            | isSameSuit (getSuits hand) =
               if sortByRank (getRanks hand) == generateStraight rank
               then
                    if rank == Numeric 10
                    then
                         RoyalFlush
                    else
                         StraightFlush
               else
                    Flush
            | sortByRank (getRanks hand) == generateStraight rank =
               Straight
            | otherwise =
               HighCard
  where
      rank = 
          if head (sortByRank (getRanks hand)) == Numeric 2 
               && last (sortByRank (getRanks hand)) == Ace
               then
                    Ace
               else
                    head (sortByRank (getRanks hand))
