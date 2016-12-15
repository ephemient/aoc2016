{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Day14 (main) where

import Control.Monad ((<=<), forM_, guard, replicateM_ )
import Control.Parallel.Strategies (parBuffer, rdeepseq, rpar, using)
import Criterion.Main (bench, bgroup, defaultMain, whnf, whnfIO)
import Crypto.Hash.MD5 (hash)
import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString, append, concatMap, unpack)
import Data.ByteString.Unsafe (unsafePackCStringLen, unsafePackMallocCStringLen, unsafeUseAsCStringLen)
import Data.Char (intToDigit, isSpace, ord)
import Data.List (dropWhileEnd, isInfixOf, tails)
import Data.Maybe (maybeToList)
import Data.String (fromString)
import Foreign (Int, Ptr, Storable(pokeElemOff, peekElemOff), copyArray, mallocBytes, allocaBytes)
import Foreign.C (CChar, CLong(CLong), CInt(CInt), CStringLen)
import Prelude hiding (concatMap, init)
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall "MD5" openssl_MD5 :: Ptr CChar -> CLong -> Ptr CChar -> IO CInt

input :: ByteString
input = fromString $ dropWhileEnd isSpace $ unsafePerformIO $ readFile "day14.txt"

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
    allocaBytes 16 $ \md -> replicateM_ n $ do
        openssl_MD5 o 32 md
        unsafePackCStringLen (md, 16)
        hexify (o, 32) (md, 16)
    unsafePackCStringLen (d, 32)

asHex :: ByteString -> ByteString
asHex = concatMap $ \w -> fromString $ map (intToDigit . fromIntegral) [w `shiftR` 4, w .&. 0xf]

hashes, hashes' :: [ByteString]
hashes = concat (map hashChunk [0, 4096..] `using` parBuffer 16 rdeepseq) where
    hashChunk from = [hash $ append input $ fromString $ show i | i <- [from .. from + 4095]]
hashes' = map ((!! 2016) . iterate (asHex . hash)) hashes `using` parBuffer 16 rpar

hashChunks, hashChunks' :: [[ByteString]]
hashChunks = map hashChunk [0, 4096..] `using` parBuffer 16 rdeepseq where
    hashChunk from = unsafePerformIO $ sequence [hashOnce $ append input $ fromString $ show i | i <- [from .. from + 4095]]
hashChunks' = map hashChunk hashChunks `using` parBuffer 16 rdeepseq where
    hashChunk chunk = unsafePerformIO $ mapM (hashRepeatedly 2016) chunk

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
main =
    --print $ keys hashes !! 63
    --print $ keys hashes' !! 63
    --print $ keys (concat hashChunks) !! 63
    --print $ keys (concat hashChunks') !! 63
    defaultMain
      [ bgroup "cryptohash"
          [ bench "hash1" $ whnf (asHex . hash) "zpqevtbw16106"
          , bench "hash2017" $ whnf ((!! 2017) . iterate (asHex . hash)) "zpqevtbw22423"
          ]
      , bgroup "openssl"
          [ bench "hash1" $ whnfIO $ hashOnce "zpqevtbw16106"
          , bench "hash2017" $ whnfIO $ hashRepeatedly 2016 =<< hashOnce "zpqevtbw22423"
          ]
      ]
