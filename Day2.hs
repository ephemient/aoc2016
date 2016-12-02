{-# LANGUAGE BangPatterns #-}
module Day2 (main) where

import Data.Array (Array, Ix, (!), assocs, bounds, inRange, ixmap, listArray)
import Data.List (find, foldl', scanl')
import Data.Maybe (mapMaybe)
import System.IO.Unsafe (unsafePerformIO)

input :: [[Direction]]
input = map (map $ read . (:[])) $ lines $ unsafePerformIO $ readFile "day2.txt"

data Direction = U | L | R | D deriving Read

keypad1, keypad2 :: Array (Int, Int) (Maybe Char)
keypad1 = listArray ((1, 1), (3, 3)) $ map Just ['1' .. '9']
keypad2 = listArray ((1, 1), (5, 5)) $ ["1", "234", "56789", "ABC", "D"] >>= center
  where center s = replicate h Nothing ++ map Just s ++ replicate t Nothing
          where h = (5 - length s) `div` 2; t = 5 - length s - h

move :: (Enum i, Ix i) => Array (i, i) (Maybe a) -> (i, i) -> Direction -> (i, i)
move keypad p@(!y, !x) = rescue . move'
  where move' U = (pred y, x)
        move' L = (y, pred x)
        move' R = (y, succ x)
        move' D = (succ y, x)
        rescue p' | bounds keypad `inRange` p', Just _ <- keypad ! p' = p' | otherwise = p

main :: IO ()
main = do
    let start keypad k | Just (p, _) <- find ((== Just k) . snd) (assocs keypad) = p
        solve keypad k = mapMaybe (keypad !) . tail . scanl' (foldl' (move keypad)) (start keypad k)
    putStrLn $ solve keypad1 '5' input
    putStrLn $ solve keypad2 '5' input
