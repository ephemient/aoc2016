{-# LANGUAGE FlexibleContexts, ViewPatterns #-}
module Day5 (main) where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import qualified Crypto.Hash.MD5 as MD5
import Data.Array.IArray (Array, IArray, Ix, accum, bounds, elems, inRange, listArray)
import Data.Bits ((.&.), shiftR, testBit)
import qualified Data.ByteString as B
import Data.Char (intToDigit, isSpace)
import Data.List (dropWhileEnd, find)
import Data.Maybe (fromJust, isJust)
import Data.String (fromString)
import Data.Word (Word8)
import Debug.Trace (trace)
import System.IO.Unsafe (unsafePerformIO)

input :: MD5.Ctx
input = MD5.update MD5.init $ fromString $ dropWhileEnd isSpace $ unsafePerformIO $ readFile "day5.txt"

hashes :: [B.ByteString]
hashes = filter hasLeadingZeros $ map hash [0..]
  where hasLeadingZeros = all (== 0) . B.zipWith (.&.) (B.pack [255, 255, 240])
        hash = MD5.finalize . MD5.update input . fromString . show

nibble :: Int -> B.ByteString -> Word8
nibble n = (if testBit n 0 then (.&. 0xf) else (`shiftR` 4)) . (`B.index` shiftR n 1)

showPassword :: (IArray a (Maybe e), Ix i, Integral e) => a i (Maybe e) -> String
showPassword = map (maybe '_' $ intToDigit . fromIntegral) . elems

updatePasswordFromHash :: (IArray a (Maybe Word8), Integral i, Ix i) => a i (Maybe Word8) -> B.ByteString -> a i (Maybe Word8)
updatePasswordFromHash pass (nibble 5 &&& nibble 6 -> (i@(fromIntegral -> i'), e)) =
    trace (intToDigit (fromIntegral i) : '*' : showPassword result) result
  where result = if inRange (bounds pass) i' then accum (<|>) pass [(i', Just e)] else pass

fillPassword :: (IArray a (Maybe Word8), Integral i, Ix i) => i -> a i (Maybe Word8)
fillPassword passLen = fromJust $ find done $ scanl updatePasswordFromHash pass0 hashes
  where pass0 = listArray (0, passLen - 1) $ repeat Nothing; done = all isJust . elems

main :: IO ()
main = do
    putStrLn $ map (intToDigit . fromIntegral) $ take 8 $ map (nibble 5) hashes
    putStrLn $ showPassword (fillPassword 8 :: Array Int (Maybe Word8))
