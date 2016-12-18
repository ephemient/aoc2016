{-|
Module      : Day17
Description : <http://adventofcode.com/2016/day/17 Day 17: Two Steps Forward>
-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day17 (main) where

import Common (readDataFile, unfold)
import Control.Monad (forM_, guard)
import Data.Bits ((.&.), shiftR)
import Data.ByteString.Char8 (ByteString, drop, index, length, putStrLn, snoc)
import Data.ByteString.Unsafe (unsafePackMallocCStringLen, unsafeUseAsCStringLen)
import Data.Char (isSpace, ord)
import Data.List (dropWhileEnd)
import Data.String (fromString)
import Foreign (Ptr, allocaBytes, mallocBytes, peekElemOff, pokeElemOff)
import Foreign.C (CChar, CLong(CLong), CInt(CInt), CStringLen)
import Prelude hiding (drop, length, putStrLn)
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall "MD5" openssl_MD5 :: Ptr CChar -> CLong -> Ptr CChar -> IO CInt

readInput :: FilePath -> IO ByteString
readInput = fmap (fromString . dropWhileEnd isSpace) . readDataFile

hexify :: CStringLen -> CStringLen -> IO ()
hexify (dest, n) (source, m) | n >= 2 * m = forM_ [0 .. m - 1] $ \i -> do
    w <- peekElemOff source i
    pokeElemOff dest (2 * i) $ integralToDigit $ w `shiftR` 4 .&. 15
    pokeElemOff dest (2 * i + 1) $ integralToDigit $ w .&. 15
  where integralToDigit c = fromIntegral (if c < 10 then ord '0' else ord 'a' - 10) + c

hash :: ByteString -> IO ByteString
hash bs = unsafeUseAsCStringLen bs $ \(d, fromIntegral -> n) -> allocaBytes 16 $ \md -> do
    openssl_MD5 d n md
    o <- mallocBytes 32
    hexify (o, 32) (md, 16)
    unsafePackMallocCStringLen (o, 32)

step :: (ByteString, (Int, Int)) -> () -> ([(ByteString, (Int, Int))], ())
step (_, (3, 3)) k = ([], k)
step (bs, (x, y)) k = (next, k) where
    open i = if index (unsafePerformIO $ hash bs) i `elem` "bcdef" then Just else const Nothing
    next = do
        Just (d, x', y') <- zipWith open [0..] [('U', x, y - 1), ('D', x, y + 1), ('L', x - 1, y), ('R', x + 1, y)]
        guard $ 0 <= x' && x' < 4 && 0 <= y' && y' < 4
        return (snoc bs d, (x', y'))

main :: IO ()
main = do
    input <- readInput "day17.txt"
    let done (_, (3, 3)) = True; done _ = False
        paths = map fst $ filter done $ unfold step () [(input, (0, 0))]
    putStrLn $ drop (length input) $ head paths
    print $ length (last paths) - length input
