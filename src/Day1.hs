{-|
Module      : Day1
Description : <http://adventofcode.com/2016/day/1 Day 1: No Time for a Taxicab>
-}
{-# LANGUAGE BangPatterns, ViewPatterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day1 (main) where

import Common (readDataFile)
import Data.List (find, scanl')
import Data.Set (empty, insert, member)
import System.IO.Unsafe (unsafePerformIO)

input :: [String]
input = split $ unsafePerformIO $ readDataFile "day1.txt"
  where split [] = []
        split s = h : split (dropWhile isSep t)
          where isSep = (`elem` ", "); (h, t) = break isSep s

data Orientation = North | East | South | West deriving (Bounded, Enum, Eq)

turnL, turnR :: Orientation -> Orientation
turnL o | o == minBound = maxBound | otherwise = pred o
turnR o | o == maxBound = minBound | otherwise = succ o

walk :: Num a => Orientation -> a -> (a, a) -> (a, a)
walk North n (x, !y) = (x, y + n)
walk East n (!x, y) = (x + n, y)
walk South n (x, !y) = (x, y - n)
walk West n (!x, y) = (x - n, y)

expand :: Enum a => Orientation -> (a, a) -> (a, a) -> [(a, a)]
expand North (x, y) (_, y') = [(x, t) | t <- [succ y .. y']]
expand East (x, y) (x', _) = [(t, y) | t <- [succ x .. x']]
expand South (x, y) (_, y') = [(x, t) | t <- reverse [y' .. pred y]]
expand West (x, y) (x', _) = [(t, y) | t <- reverse [x' .. pred x]]

loop :: (Num a, Read a) => (Orientation, (a, a)) -> String -> (Orientation, (a, a))
loop (o, p) ('L':n) = let o' = turnL o in (o', walk o' (read n) p)
loop (o, p) ('R':n) = let o' = turnR o in (o', walk o' (read n) p)

main :: IO ()
main = do
    let (_:os, ps@(last -> (x, y))) = unzip $ scanl' loop (North, (0, 0)) input
        expanded = (0, 0) : concat (zipWith3 expand os ps (tail ps))
        visited = scanl' (flip insert) empty expanded
        Just ((x', y'), _) = find (uncurry member) $ zip expanded visited
    print $ abs x + abs y
    print $ abs x' + abs y'
