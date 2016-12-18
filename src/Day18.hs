{-|
Module      : Day18
Description : <http://adventofcode.com/2016/day/18 Day 18: Like a Rogue>
-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day18 (main) where

import System.IO.Unsafe (unsafePerformIO)

input :: [Bool]
input = map (== '.') $ filter (`elem` ".^") $ unsafePerformIO $ readFile "day18.txt"

next :: [Bool] -> [Bool]
next line = zipWith (==) (True : line) (tail line ++ [True])

main :: IO ()
main = do
    let full = iterate next input
    print $ length $ filter id $ concat $ take 40 full
    print $ length $ filter id $ concat $ take 400000 full
