{-|
Module      : Day3
Description : <http://adventofcode.com/2016/day/3 Day 3: Squares With Three Sides>
-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day3 (main) where

import Common (readDataFile)
import Data.List (tails, transpose)
import System.IO.Unsafe (unsafePerformIO)

input, input2 :: (Read a) => [[a]]
input = map (map read . words) $ lines $ unsafePerformIO $ readDataFile "day3.txt"
input2 = threes $ concat $ transpose input
  where threes [] = []; threes (splitAt 3 -> (h, t)) = h : threes t

isTriangle :: (Num a, Ord a) => [a] -> Bool
isTriangle s = and [sum s - n > n | n <- s]

main :: IO ()
main = do
    print $ length $ filter isTriangle input
    print $ length $ filter isTriangle input2
