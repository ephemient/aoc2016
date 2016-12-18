{-# LANGUAGE FlexibleContexts, ViewPatterns #-}
module Day5 (main) where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Data.Array.IArray (Array, IArray, Ix, accum, bounds, elems, inRange, listArray)
import Data.Bits ((.&.), shiftR, testBit)
import Data.ByteString (ByteString, append, index, packCStringLen)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Char (intToDigit, isSpace)
import Data.List (dropWhileEnd)
import Data.Maybe (isJust, mapMaybe)
import Data.String (fromString)
import Data.Word (Word8)
import Debug.Trace (trace)
import Foreign (Ptr, allocaBytes, peekElemOff)
import Foreign.C (CChar, CLong(CLong), CInt(CInt))
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall "MD5" openssl_MD5 :: Ptr CChar -> CLong -> Ptr CChar -> IO CInt

readInput :: FilePath -> IO ByteString
readInput = fmap (fromString . dropWhileEnd isSpace) . readFile

hashes :: ByteString -> [ByteString]
hashes input = concat filteredChunks where
    filteredChunks = map filteredChunkFrom [0, 4096..]
    filteredChunkFrom from = mapMaybe hash [from .. from + 4095]
    hash i = unsafePerformIO $ unsafeUseAsCStringLen (append input $ fromString $ show i) $
        \(d, fromIntegral -> n) -> allocaBytes 16 $ \md -> do
            openssl_MD5 d n md
            b0 <- peekElemOff md 0; b1 <- peekElemOff md 1; b2 <- peekElemOff md 2
            if b0 == 0 && b1 == 0 && b2 .&. 240 == 0
            then Just <$> packCStringLen (md, 16) else return Nothing

nibble :: Int -> ByteString -> Word8
nibble n = (if testBit n 0 then (.&. 0xf) else (`shiftR` 4)) . (`index` shiftR n 1)

showPassword :: (IArray a (Maybe e), Ix i, Integral e) => a i (Maybe e) -> String
showPassword = map (maybe '_' $ intToDigit . fromIntegral) . elems

updatePasswordFromHash :: (IArray a (Maybe Word8), Integral i, Ix i) => a i (Maybe Word8) -> ByteString -> a i (Maybe Word8)
updatePasswordFromHash pass (nibble 5 &&& nibble 6 -> (i@(fromIntegral -> i'), e)) =
    trace (intToDigit (fromIntegral i) : '*' : showPassword result) result
  where result = if inRange (bounds pass) i' then accum (<|>) pass [(i', Just e)] else pass

fillPassword :: (IArray a (Maybe Word8), Integral i, Ix i) => [ByteString] -> i -> a i (Maybe Word8)
fillPassword hashes passLen = head $ filter done $ scanl updatePasswordFromHash pass0 hashes
  where pass0 = listArray (0, passLen - 1) $ repeat Nothing; done = all isJust . elems

main :: IO ()
main = do
    input <- readInput "day5.txt"
    let hashInput = hashes input
    putStrLn $ map (intToDigit . fromIntegral) $ take 8 $ map (nibble 5) hashInput
    putStrLn $ showPassword (fillPassword hashInput 8 :: Array Int (Maybe Word8))
