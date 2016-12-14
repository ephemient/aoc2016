{-|
Module      : Day14
Description : <http://adventofcode.com/2016/day/14 Day 14: One-Time Pad>
-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day14 (main) where

import Common (readDataFile)
import Control.Monad (guard)
import Control.Parallel.Strategies (parBuffer, rdeepseq, rpar, using)
import Crypto.Hash.MD5 (Ctx, finalize, hash, init, update)
import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString, unpack, concatMap)
import Data.Char (intToDigit, isSpace)
import Data.List (dropWhileEnd, isInfixOf, tails)
import Data.Maybe (maybeToList)
import Data.String (fromString)
import Prelude hiding (concatMap, init)
import System.IO.Unsafe (unsafePerformIO)

input :: Ctx
input = update init $ fromString $ dropWhileEnd isSpace $ unsafePerformIO $ readDataFile "day14.txt"

asHex :: ByteString -> ByteString
asHex = concatMap $ \w -> fromString $ map (intToDigit . fromIntegral) [w `shiftR` 4, w .&. 0xf]

hashes, hashes' :: [ByteString]
hashes = concat (map hashChunk [0, 4096..] `using` parBuffer 16 rdeepseq) where
    hashChunk from = map (asHex . finalize . update input . fromString . show) [from .. from + 4095]
hashes' = map ((!! 2016) . iterate (asHex . hash)) hashes `using` parBuffer 16 rpar

keys :: [ByteString] -> [Int]
keys hashes = do
    (i, first : rest) <- zip [0..] $ tails hashes
    let findTriple (a:b:c:_) | a == b && b == c = Just a
        findTriple (_:s) = findTriple s
        findTriple _ = Nothing
    t <- maybeToList $ findTriple $ unpack first
    let hasQuintuple s = replicate 5 t `isInfixOf` unpack s
    guard $ any hasQuintuple $ take 1000 rest
    return i

main :: IO ()
main = do
    print $ keys hashes !! 63
    print $ keys hashes' !! 63
