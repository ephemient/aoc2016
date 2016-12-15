{-|
Module      : Day15
Description : <http://adventofcode.com/2016/day/15 Day 15: Timing is Everything>
-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day15 (main) where

import Data.Char (isDigit, isSpace)
import Text.ParserCombinators.ReadP (between, char, many, munch1, readP_to_S, string)

readInput :: (Integral a, Read a) => FilePath -> IO [(a, a)]
readInput = fmap getParse . readFile where
    getParse = fst . head . filter (null . snd) . readP_to_S parse
    parse = many $ do
        d <- string "Disc #" >> read <$> munch1 isDigit
        q <- string " has " >> read <$> munch1 isDigit
        r <- string " positions; at time=0, it is at position " >> read <$> munch1 isDigit
        char '.'; munch1 isSpace
        return ((r + d) `mod` q, q)

combine :: (Integral a) => (a, a) -> (a, a) -> (a, a)
combine (!r1, !q1) (!r2, !q2) = (r3 `mod` q3, q3) where
    q3 = lcm q1 q2
    r3 = common [r1, r1 + q1 ..] [r2, r2 + q2 ..]
    common xs@(x:xs') ys@(y:ys') = case compare x y of
        EQ -> x
        LT -> common xs' ys
        GT -> common xs ys'

main :: IO ()
main = do
    input <- readInput "day15.txt"
    print $ uncurry subtract $ foldr1 combine input
    print $ uncurry subtract $ foldr combine ((length input + 1) `mod` 11, 11) input
