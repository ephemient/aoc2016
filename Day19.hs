module Day19 (main) where

import Data.Bits (FiniteBits, clearBit, countLeadingZeros, finiteBitSize, setBit, shiftL)
import System.IO.Unsafe (unsafePerformIO)

input :: Int
input = read $ unsafePerformIO $ readFile "day19.txt"

josephus :: (FiniteBits a) => a -> a
josephus n = ((n `shiftL` 1) `setBit` 0) `clearBit` (finiteBitSize n - countLeadingZeros n)

josephus' :: (Integral a) => a -> a
josephus' n = n - min m (3 * m - n) where
    m = 3 ^ length (takeWhile (>= 3) $ iterate (`div` 3) $ n - 1)

main :: IO ()
main = do
    print $ josephus input
    print $ josephus' input
