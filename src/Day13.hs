{-|
Module      : Day13
Description : <http://adventofcode.com/2016/day/13 Day 13: A Maze of Twisty Little Cubicles>
-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day13 (main) where

import Common (readDataFile, unfold)
import Data.Bits (Bits, popCount, testBit)
import Data.Set (Set, insert, member, singleton)
import System.IO.Unsafe (unsafePerformIO)

input :: (Num a, Read a) => a
input = read $ unsafePerformIO $ readFile "day13.txt"

wall :: (Bits a, Num a) => a -> (a, a) -> Bool
wall input (x, y) = popCount (x^2 + 3*x + 2*x*y + y + y^2 + input) `testBit` 0

walk :: (Bits a, Num a, Ord a, Enum b) =>
    a -> ((a, a), b) -> Set (a, a) -> ([((a, a), b)], Set (a, a))
walk input ((x, y), n) visited = (map (, succ n) next, foldr insert visited next) where
    next = filter ok [(x + 1, y), (x, y - 1), (x - 1, y), (x, y + 1)]
    ok (x, y) = x >= 0 && y >= 0 && not (wall input (x, y) || member (x, y) visited)

main :: IO ()
main = do
    let reach = unfold (walk input) (singleton (1, 1)) [((1, 1), 0)] :: [((Int, Int), Int)]
    print $ snd $ head $ filter ((==) (31, 39) . fst) reach
    print $ length $ takeWhile ((<= 50) . snd) reach
