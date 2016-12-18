{-# LANGUAGE ViewPatterns #-}
module Day14 (main) where

import Control.Monad (forM_, guard, replicateM_ )
import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString, append, isInfixOf, replicate, uncons)
import Data.ByteString.Unsafe (unsafePackMallocCStringLen, unsafeUseAsCStringLen)
import Data.Char (isSpace, ord)
import Data.List (dropWhileEnd, tails)
import Data.Maybe (maybeToList)
import Data.String (fromString)
import Foreign (Ptr, allocaBytes, copyArray, mallocBytes, peekElemOff, pokeElemOff)
import Foreign.C (CChar, CLong(CLong), CInt(CInt), CStringLen)
import Prelude hiding (replicate)
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall "MD5" openssl_MD5 :: Ptr CChar -> CLong -> Ptr CChar -> IO CInt

readInput :: FilePath -> IO ByteString
readInput = fmap (fromString . dropWhileEnd isSpace) . readFile

hexify :: CStringLen -> CStringLen -> IO ()
hexify (dest, n) (source, m) | n >= 2 * m = forM_ [0 .. m - 1] $ \i -> do
    w <- peekElemOff source i
    pokeElemOff dest (2 * i) $ integralToDigit $ w `shiftR` 4 .&. 15
    pokeElemOff dest (2 * i + 1) $ integralToDigit $ w .&. 15
  where integralToDigit c = fromIntegral (if c < 10 then ord '0' else ord 'a' - 10) + c

hashOnce :: ByteString -> IO ByteString
hashOnce bs = unsafeUseAsCStringLen bs $ \(d, fromIntegral -> n) -> allocaBytes 16 $ \md -> do
    openssl_MD5 d n md
    o <- mallocBytes 32
    hexify (o, 32) (md, 16)
    unsafePackMallocCStringLen (o, 32)

hashRepeatedly :: Int -> ByteString -> IO ByteString
hashRepeatedly n bs = unsafeUseAsCStringLen bs $ \(d, 32) -> do
    o <- mallocBytes 32
    copyArray o d 32
    allocaBytes 16 $ \md -> replicateM_ n $ openssl_MD5 o 32 md >> hexify (o, 32) (md, 16)
    unsafePackMallocCStringLen (o, 32)

hashes :: ByteString -> [ByteString]
hashes input = map (unsafePerformIO . hashOnce . append input . fromString . show) [0..]

rehash :: [ByteString] -> [ByteString]
rehash = map $ unsafePerformIO . hashRepeatedly 2016

keys :: [ByteString] -> [Int]
keys hashes = do
    (i, first : rest) <- zip [0..] $ tails hashes
    let findTriple (uncons -> Just (a, s@(uncons -> Just (b, uncons -> Just (c, _)))))
          | a == b && b == c = Just a
          | otherwise = findTriple s
        findTriple _ = Nothing
    guard $ maybe False (\t -> any (isInfixOf $ replicate 5 t) $ take 1000 rest) $ findTriple first
    return i

main :: IO ()
main = do
    input <- readInput "day14.txt"
    let hashInput = hashes input
    print $ keys hashInput !! 63
    print $ keys (rehash hashInput) !! 63
