{-# LANGUAGE ViewPatterns #-}
module Day20 (main) where

import Data.Char (isDigit)
import Data.List (sort)
import Data.Ord (comparing)
import System.IO.Unsafe (unsafePerformIO)

input :: (Num a, Read a) => [(a, a)]
input = map splitRead $ lines $ unsafePerformIO $ readFile "day20.txt"
  where splitRead (span isDigit -> (a, '-':b)) | all isDigit b = (read a, read b)

gaps :: (Num a, Ord a) => a -> [(a, a)] -> a -> [(a, a)]
gaps last ((start, end) : intervals) finish =
    (if last < start then (:) (last, start) else id) $ gaps (end + 1) intervals finish
gaps last [] finish = [(last, finish) | last < finish]

main :: IO ()
main = do
    let whitelist = gaps 0 (sort input) (2 ^ 32)
    print $ fst $ head whitelist
    print $ sum $ map (uncurry subtract) whitelist
