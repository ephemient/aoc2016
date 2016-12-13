{-# LANGUAGE TupleSections #-}
module Day13 (main) where

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

unfold :: (a -> b -> ([a], b)) -> b -> [a] -> [a]
unfold _ _ [] = []
unfold f k as = as' ++ unfold f k' as' where
    unfold' k [] = ([], k)
    unfold' k (a:as) = let (as', k') = f a k; ~(as'', k'') = unfold' k' as in (as' ++ as'', k'')
    (as', k') = unfold' k as

main :: IO ()
main = do
    let reach = unfold (walk input) (singleton (1, 1)) [((1, 1), 0)] :: [((Int, Int), Int)]
    print $ snd $ head $ filter ((==) (31, 39) . fst) reach
    print $ succ $ length $ takeWhile ((<= 50) . snd) reach
