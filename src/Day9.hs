{-|
Module      : Day9
Description : <http://adventofcode.com/2016/day/9 Day 9: Explosives in Cyberspace>
-}
{-# LANGUAGE BangPatterns, ViewPatterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day9 (main) where

import Common (readDataFile)
import Data.Char (isDigit, isSpace)
import Data.List (genericDrop, genericLength)
import System.IO.Unsafe (unsafePerformIO)

input :: String
input = filter (not . isSpace) $ unsafePerformIO $ readDataFile "day9.txt"

v1 :: (Integral a, Read a) => String -> a
v1 = v1' 0 where
    v1' k [] = k
    v1' !k ('(' :
            (span isDigit ->
             (read -> len, 'x' :
              (span isDigit ->
               (read -> count, ')' :
                (genericDrop len -> s))))))
      = v1' (k + count * len) s
    v1' !k (span (/= '(') -> (genericLength -> c, s)) = v1' (k + c) s

v2 :: (Integral a, Read a) => String -> a
v2 = v2' 0 where
    v2' k [] = k
    v2' !k ('(' :
            (span isDigit ->
             (read -> len, 'x' :
              (span isDigit ->
               (read -> count, ')' :
                (splitAt len ->
                 (v2' 0 -> c, s)))))))
      = v2' (k + count * c) s
    v2' !k (span (/= '(') -> (genericLength -> c, s)) = v2' (k + c) s

main :: IO ()
main = do
    print $ v1 input
    print $ v2 input
