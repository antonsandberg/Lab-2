module BlackJack where
import Cards
import RunGame
import Test.QuickCheck


-------------------------------------------------------------------------
-- A0
-------------------------------------------------------------------------
-- Defining the hand2 as in the Assignment
hand2 = Add (Card (Numeric 2) Hearts)
                (Add (Card Jack Spades) Empty)
hand3 = Add (Card (Numeric 5) Hearts)
                (Add (Card Jack Spades) Empty)
card2 = Card (Numeric 2) Hearts

acesHand = Add (Card Ace Hearts)
                (Add (Card Ace Spades) Empty)

-- Completing the sequence by recursively calculate the size
-- Answer should be [2, 2, 2, 2, 2]
sizeSteps :: [Integer]
sizeSteps = [size hand2
            , size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))
                        , 1 + size (Add (Card Jack Spades) Empty)
                        , 1 + 1 + size Empty
                        , 1 + 1 + 0
                        , 2]
-------------------------------------------------------------------------
-- A1
-------------------------------------------------------------------------
-- Temporary help functions since I suck, want to look into this
displaySuit :: Card -> String
displaySuit (Card _ Hearts) = "Hearts" --"u\9829"
displaySuit (Card _ Spades) = "Spades" --"u\9824"
displaySuit (Card _ Diamonds) = "Diamonds" --"u\9830"
displaySuit (Card _ Clubs) = "Clubs" --"u\9827"


displayRank :: Card -> String
displayRank (Card Ace _) = "Ace"
displayRank (Card King _) = "King"
displayRank (Card Queen _) = "Queen"
displayRank (Card Jack _) = "Jack"
displayRank (Card (Numeric r) _) = show r


-- Adding rank and suit together with (++)
displayCard :: Card -> String
displayCard c =  displayRank c ++ " of " ++ displaySuit c ++ "\n"

-- Using displayCard to add together the hand
displayHand' :: Hand -> String
displayHand' Empty = ""
displayHand' (Add c h) = displayCard c ++ displayHand' h 
-------------------------------------------------------------------------
-- A2
-------------------------------------------------------------------------
-- Calculating value, Assume Ace = 11
initialValue :: Hand -> Integer
initialValue Empty = 0 
initialValue (Add c h) = valueRank (rank c) + initialValue h

-- Simple function to determine each cards numeric value
valueRank :: Rank -> Integer
valueRank (Numeric r) = r
valueRank Ace = 11
valueRank _ = 10  

-- Simple recursive way to check number of aces
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace _) h) = 1 + numberOfAces h
numberOfAces (Add _ h) = 0 + numberOfAces h

-- Use our numberOfAces function to compute our correct value
value :: Hand -> Integer
value Empty = 0
value h  | initialValue h > 20 = initialValue h - 10*numberOfAces h
            | otherwise = initialValue h

-------------------------------------------------------------------------
-- A3
-------------------------------------------------------------------------
-- Check if a hand is bust
gameOver :: Hand -> Bool
gameOver h  | value h > 20 = False  
            | otherwise = False


-------------------------------------------------------------------------
-- A4
-------------------------------------------------------------------------
-- Check if the guest or the bank has won, by using the rules of the game
-- If guest is bust -> Bank
-- If bank is bust and guest isn't -> Bank
-- Now we know that neither are bust: can check values
-- The guest's only win condition is if it has a strictly higher score -> Guest
-- otherwise we have that -> Bank

winner :: Hand -> Hand -> Player
winner guest _     | gameOver guest = Bank
winner guest bank  | not (gameOver guest) && gameOver bank = Guest              
                   | value guest > value bank = Guest
                   | otherwise = Bank