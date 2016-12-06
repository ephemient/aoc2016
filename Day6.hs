module Day6 (main) where

import Data.List (group, maximumBy, minimumBy, sort, transpose)
import Data.Ord (comparing)
import System.IO.Unsafe (unsafePerformIO)

input :: [String]
input = lines $ unsafePerformIO $ readFile "day6.txt"

common, uncommon :: (Ord a) => [a] -> a
common = head . maximumBy (comparing length) . group . sort
uncommon = head . minimumBy (comparing length) . group . sort

main :: IO ()
main = do
    putStrLn $ map common $ transpose input
    putStrLn $ map uncommon $ transpose input
