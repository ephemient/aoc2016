{-|
Module      : Day16
Description : <http://adventofcode.com/2016/day/16 Day 16: Dragon Checksum>
-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day16 (main) where

import System.IO.Unsafe (unsafePerformIO)

input :: String
input = filter (`elem` "01") $ unsafePerformIO $ readFile "day16.txt"

fill :: String -> String
fill a = let a' = a ++ '0' : map invert (reverse a) in a' ++ fill' a' where
    fill' a' = b ++ fill' a'' where a'' = a' ++ b; b = '0' : map invert (reverse a')
    invert '0' = '1'; invert _ = '0'

cks :: Int -> String -> String
cks 0 = error; cks l = case quotRem l 2 of (l', 0) -> cks l' . ck; _ -> take l
  where ck (a:b:s) = (if a == b then '1' else '0') : ck s; ck _ = []

main :: IO ()
main = do
    let dragon = fill input
    putStrLn $ cks 272 dragon
    putStrLn $ cks 35651584 dragon
