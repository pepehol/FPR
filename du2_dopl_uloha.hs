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

countOccurence :: Hand -> Card -> Int
countOccurence [] _ = 0
countOccurence (x:xs) card = if x == card then 1 + countOccurence xs card else countOccurence xs card 

-- TESTOVACI FUNKCE.

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

-- Flush
test6 :: [Card]
test6 = [Card (Numeric 2) Hearts,Card (Numeric 5) Hearts,Card (Numeric 3) Hearts,Card (Numeric 10) Hearts,Card (Numeric 7) Hearts]

-- RoyalFlush
test7 :: [Card]
test7 = [Card Ace Hearts,Card King Hearts,Card Queen Hearts,Card (Numeric 10) Hearts,Card Jack Hearts]

-- Four
test8 :: [Card]
test8 = [Card Ace Hearts,Card Ace Hearts,Card Ace Hearts,Card (Numeric 10) Hearts,Card Ace Hearts]