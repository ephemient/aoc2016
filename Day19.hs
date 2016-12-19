module Day19 (main) where

import Data.Bits (FiniteBits, clearBit, countLeadingZeros, finiteBitSize, setBit, shiftL)
import Data.Set (deleteAt, findMin, fromList, size)
import System.IO.Unsafe (unsafePerformIO)

input :: Int
input = read $ unsafePerformIO $ readFile "day19.txt"

josephus :: (FiniteBits a) => a -> a
josephus n = ((n `shiftL` 1) `setBit` 0) `clearBit` (finiteBitSize n - countLeadingZeros n)

josephus' :: (Enum a, Num a, Ord a) => a -> a
josephus' = steal 0 . fromList . enumFromTo 1 where
    steal i elves
      | size elves <= 1 = findMin elves
      | otherwise = steal ((if j < i then i else i + 1) `mod` size elves') elves' where
            j = (i + size elves `div` 2) `mod` size elves
            elves' = deleteAt j elves

main :: IO ()
main = do
    print $ josephus input
    print $ josephus' input
