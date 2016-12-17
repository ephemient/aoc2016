{-|
Module      : Day17
Description : <http://adventofcode.com/2016/day/17 Day 17: Two Steps Forward>
-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day17 (main) where

import Common (readDataFile, unfold)
import Control.Monad (guard)
import Crypto.Hash.MD5 (hash)
import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString, append, concatMap, drop, length, unpack)
import Data.ByteString.Char8 (index, putStrLn, snoc)
import Data.Char (intToDigit, isSpace)
import Data.List (dropWhileEnd)
import Data.String (fromString)
import Prelude hiding (concatMap, drop, length, putStrLn)
import System.IO.Unsafe (unsafePerformIO)

input :: ByteString
input = fromString $ dropWhileEnd isSpace $ unsafePerformIO $ readDataFile "day17.txt"

asHex :: ByteString -> ByteString
asHex = concatMap $ \w -> fromString $ map (intToDigit . fromIntegral) [w `shiftR` 4, w .&. 0xf]

step :: (ByteString, (Int, Int)) -> () -> ([(ByteString, (Int, Int))], ())
step (_, (3, 3)) k = ([], k)
step (bs, (x, y)) k = (next, k) where
    open i = if index (asHex $ hash bs) i `elem` "bcdef" then Just else const Nothing
    next = do
        Just (d, x', y') <- zipWith open [0..] [('U', x, y - 1), ('D', x, y + 1), ('L', x - 1, y), ('R', x + 1, y)]
        guard $ 0 <= x' && x' < 4 && 0 <= y' && y' < 4
        return (snoc bs d, (x', y'))

main :: IO ()
main = do
    let done (_, (3, 3)) = True; done _ = False
        paths = map fst $ filter done $ unfold step () [(input, (0, 0))]
    putStrLn $ drop (length input) $ head paths
    print $ length (last paths) - length input
