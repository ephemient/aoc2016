{-|
Module      : Day20
Description : <http://adventofcode.com/2016/day/20 Day 20: Firewall Rules>
-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day20 (main) where

import Common (readDataFile)
import Control.Arrow (second)
import Data.Char (isDigit)
import Data.Sequence (Seq, (!?), (<|), (><), (|>), index, singleton)
import qualified Data.Sequence as Seq
import System.IO.Unsafe (unsafePerformIO)

input :: [(Int, Int)]
input = map splitRead $ lines $ unsafePerformIO $ readDataFile "day20.txt"
  where splitRead (span isDigit -> (a, '-':b)) | all isDigit b = (read a, read b)

bsearch :: (a -> Ordering) -> Seq a -> Int
bsearch f s = bsearch' 0 (length s) where
    bsearch' lo hi
      | lo < hi, Just LT <- f <$> s !? mid = bsearch' lo mid
      | lo < hi, Just GT <- f <$> s !? mid = bsearch' (mid + 1) hi
      | otherwise = lo
      where mid = lo + (hi - lo) `div` 2

merge :: (Ord a) => (a, a) -> Seq (a, a) -> Seq (a, a)
merge (start, end) intervals = before >< singleton (start', end') >< after where
    lo = bsearch (compare start . fst) intervals
    hi = bsearch (compare end . snd) intervals
    (start', lo') = case intervals !? (lo - 1) of
        Just (prevStart, prevEnd) | prevEnd >= start -> (prevStart, lo - 1)
        _ -> (start, lo)
    (end', hi') = case intervals !? hi of
        Just (nextStart, nextEnd) | nextStart <= end -> (nextEnd, hi + 1)
        _ -> (end, hi)
    before = Seq.take lo' intervals
    after = Seq.drop hi' intervals

invert :: a -> a -> Seq (a, a) -> Seq (a, a)
invert start end seq = Seq.zip (start <| fmap snd seq) (fmap fst seq |> end)

main :: IO ()
main = do
    let intervals = second succ <$> foldr merge Seq.empty input
        gaps = Seq.filter (uncurry (<)) $ invert 0 (2 ^ 32) intervals
    print $ fst $ index gaps 0
    print $ foldr ((+) . uncurry subtract) 0 gaps
