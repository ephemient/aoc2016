{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module Day10 (main) where

import Data.Char (isDigit)
import Data.Either (partitionEithers)
import Data.List (sort)
import Data.Map.Strict (Map, assocs, empty, insert, insertWith, lookup)
import Prelude hiding (lookup)
import System.IO.Unsafe (unsafePerformIO)
import Text.ParserCombinators.ReadP (ReadP, (+++), munch, readP_to_S, string)

input :: [String]
input = lines $ unsafePerformIO $ readFile "day10.txt"

data Initial i e = Initial { bot :: i, value :: e }
data Instruction i j = Instruction { src :: i, low :: Either i j, high :: Either i j }

parse :: (Read i, Read j, Read e) => ReadP (Either (Initial i e) (Instruction i j))
parse = (Left <$> initial) +++ (Right <$> instruction) where
    initial = do
        value <- string "value " >> read <$> munch isDigit
        bot <- string " goes to bot " >> read <$> munch isDigit
        return Initial {..}
    instruction = do
        src <- parseBot <* string " gives low to "
        low <- (Left <$> parseBot) +++ (Right <$> parseOutput)
        string " and high to "
        high <- (Left <$> parseBot) +++ (Right <$> parseOutput)
        return Instruction {..}
    parseBot = string "bot " >> read <$> munch isDigit
    parseOutput = string "output " >> read <$> munch isDigit

insertSorted :: (Ord i, Ord e) => e -> i -> Map i [e] -> Map i [e]
insertSorted e i = insertWith ((.) sort . (++)) i [e]

initialize :: (Ord i, Ord e) => [Initial i e] -> Map i [e]
initialize = foldr (\Initial {..} -> insertSorted value bot) empty

step :: (Ord i, Ord j, Ord e) =>
    [Instruction i j] -> (Map i [e], Map j [e]) -> (Map i [e], Map j [e])
step instructions (bots, outputs) = foldr emit (bots', outputs) targets where
    (foldr (uncurry insert) bots -> bots', concat -> targets) = unzip
      [ ((bot, rest), [(low, value0), (high, value1)])
      | Instruction {src = bot@((`lookup` bots) -> Just (value0:value1:rest)), ..} <- instructions
      ]
    emit (Left bot, value) (bots, outputs) = (insertSorted value bot bots, outputs)
    emit (Right output, value) (bots, outputs) = (bots, insertSorted value output outputs)

main :: IO ()
main = do
    let get (readP_to_S parse -> (i, ""):_) = i :: Either (Initial Int Int) (Instruction Int Int)
        (setup, instructions) = partitionEithers $ map get input
        (bots, outputs) = unzip $ iterate (step instructions) (initialize setup, empty)
    print $ fst $ head $ bots >>= filter ((==) [17, 61] . snd) . assocs
    print $ head
      [ value0 * value1 * value2
      | Just (value0:_) <- lookup 0 <$> outputs
      , Just (value1:_) <- lookup 1 <$> outputs
      , Just (value2:_) <- lookup 2 <$> outputs
      ]
