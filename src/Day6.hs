{-|
Module      : Day6
Description : <http://adventofcode.com/2016/day/6 Day 6: Signals and Noise>
-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day6 (main) where

import Common (readDataFile)
import Data.List (group, maximumBy, minimumBy, sort, transpose)
import Data.Ord (comparing)
import System.IO.Unsafe (unsafePerformIO)

input :: [String]
input = lines $ unsafePerformIO $ readDataFile "day6.txt"

common, uncommon :: (Ord a) => [a] -> a
common = head . maximumBy (comparing length) . group . sort
uncommon = head . minimumBy (comparing length) . group . sort

main :: IO ()
main = do
    putStrLn $ map common $ transpose input
    putStrLn $ map uncommon $ transpose input
