{-# LANGUAGE TupleSections #-}
 
module Main where

import Control.Monad.Random
import Data.Array.IO
import qualified Data.Map.Strict as Map
import Control.Arrow
import Data.List
import Data.Ord

data Suit = Club | Spade | Diamond | Heart deriving (Eq, Ord)

instance Show Suit where
  show Club = "♠"
  show Spade = "♣"
  show Diamond = "♦"
  show Heart = "♥"

data Card = Card Suit Int deriving (Eq, Ord)

isSuit :: Suit -> Card -> Bool
isSuit suit (Card suit' _) = suit == suit'

isRank :: Int -> Card -> Bool
isRank rank (Card _ rank') = rank == rank'

instance Show Card where
  show (Card suit rank) = rank' <> " " <> suit'
    where suit' = show suit
          rank' = case rank of
                  1 -> "A"
                  11 -> "J"
                  12 -> "Q"
                  13 -> "K"
                  x -> show x

type Deck = [Card]

mkDeck :: (RandomGen g) => RandT g IO Deck
mkDeck = do
  let suits = [Club, Spade, Diamond, Heart]
      ranks = [1..13]
      deck = [Card suit rank | rank <- ranks, suit <- suits]

  pure deck

-- https://wiki.haskell.org/Random_shuffle#Imperative_algorithm
shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- mkNew n xs
  forM [1..n] $ \i -> do
      j <- randomRIO (i,n)
      vi <- readArray ar i
      vj <- readArray ar j
      writeArray ar j vi
      return vj
  where
    n = length xs
    mkNew :: Int -> [a] -> IO (IOArray Int a)
    mkNew n' xs' =  newListArray (1,n') xs'

draw :: Int -> Deck -> Maybe ([Card], Deck)
draw 0 [] = Just ([], [])
draw _ [] = Nothing
draw cards deck
  | cards > length deck = Nothing
  | otherwise = Just (take cards deck, drop cards deck)

suitCounts :: [Card] -> [(Suit, Int)]
suitCounts hand = (id *** length) <$> pairs
  where pairs = Map.assocs $ partitionSuits hand

maxSuit :: [Card] -> Suit
maxSuit hand = fst $ maximumBy (comparing snd) (suitCounts hand)

partitionSuits :: [Card] -> Map.Map Suit [Card]
partitionSuits hand =
  Map.fromListWith (flip (++)) [(suit, [card]) | card@(Card suit _) <- hand]

hasFlush :: [Card] -> Bool
hasFlush hand = maxSuitCount >= 5
  where maxSuitCount = maximum suits
        suits = length <$> Map.elems partitioned
        partitioned = partitionSuits hand

flushStrat :: Int -> [Card] -> Deck -> Maybe (Int, [Card])
flushStrat discards hand deck
  | hasFlush hand = Just (discards, hand)
  | otherwise = do
    let chosenSuit = maxSuit hand
        hand' = filter (isSuit chosenSuit) hand
        discarded = (length hand) - (length hand')
    (cards, deck') <- draw discarded deck
    flushStrat (discards + 1) (hand' <> cards) deck'

simulateStrat :: IO (Maybe (Int, [Card]))
simulateStrat = do
  g <- getStdGen
  deck <- shuffle =<< fst <$> runRandT mkDeck g

  pure $ do
    (hand, deck') <- draw 8 deck
    flushStrat 0 hand deck'

main :: IO ()
main = do
  let rounds = 10000

  results <- sequence $ take rounds (repeat simulateStrat)

  let toCount Nothing = 999
      toCount (Just (x, _)) = x
      counts = toCount <$> results
      histogram :: Map.Map Int Int
      histogram = Map.fromListWith (+) $ (,1) <$> counts

  putStrLn $ show histogram
