module Pokerhand where

import Data.List

data Suit = Diamond | Clubs
  deriving (Eq, Show)

instance Ord Suit where
  _ `compare` _ = EQ

data Rank = VIII | IX | X | Jack | Queen | King | Ace 
  deriving (Eq, Show, Ord)

data Card = Card Rank Suit 
  deriving (Show)

suit :: Card -> Suit
suit (Card r s) = s

rank :: Card -> Rank
rank (Card r s) = r

instance Eq Card where
  a == b = (rank a) == (rank b)

instance Ord Card where
  a `compare` b = (rank a) `compare` (rank b)

data Hand = Hand Card Card Card Card Card
  deriving (Show)

instance Eq Hand where
  a == b = (sortedcards a) == (sortedcards b)

instance Ord Hand where
  a `compare` b = (combination a) `compare` (combination b)

sortedcards :: Hand -> [Card]
sortedcards (Hand a b c d e) = (reverse.sort) [a, b, c, d, e]

data Combination = High [Card] 
                 | Pair [Card]
                 | Double [Card]
                 | Triple [Card]
                 | Color [Card]
  deriving (Eq, Show, Ord)

combination hand
  | isColor hand  = Color $ colorCard hand ++ restFrom hand 
  | isTriple hand = Triple $ tripleCard hand ++ restFrom hand 
  | isDouble hand = Double $ pairCard hand ++ restFrom hand 
  | isPair hand   = Pair $ pairCard hand ++ restFrom hand 
  | otherwise     = High (sortedcards hand) 

isColor :: Hand -> Bool
isColor hand = length(colorsIn hand) == 1

isTriple :: Hand -> Bool
isTriple hand = length(triplesIn hand) == 1

isPair :: Hand -> Bool
isPair hand = length(pairsIn hand) == 1

isDouble :: Hand -> Bool
isDouble hand = length(pairsIn hand) == 2

pairCard = concat . pairsIn 

tripleCard = concat . triplesIn 

colorCard = concat . colorsIn 

keepOnlyPairs cardGroup = length cardGroup == 2

keepOnlyTriples cardGroup = length cardGroup == 3

keepOnlyColors cardGroup = length cardGroup == 5

colorsIn :: Hand -> [[Card]]
colorsIn hand = filter keepOnlyColors (groupBy eqSuit cards)
  where cards = sortedcards hand 

triplesIn :: Hand -> [[Card]]
triplesIn hand = filter keepOnlyTriples (group cards)
  where cards = sortedcards hand 

eqSuit :: Card -> Card -> Bool
eqSuit a b = (suit a) == (suit b)

pairsIn :: Hand -> [[Card]]
pairsIn hand = filter keepOnlyPairs (group cards)
  where cards = sortedcards hand 

restFrom hand =concat $ filter (\l -> length l == 1) 
                               (group (sortedcards hand))



