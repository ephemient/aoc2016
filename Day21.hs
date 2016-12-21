{-# LANGUAGE FlexibleInstances, GADTs, ViewPatterns #-}
module Day21 (main) where

import Control.Monad (liftM2)
import Data.Char (isAlpha, isDigit)
import Data.List (elemIndex, foldl')
import System.IO.Unsafe (unsafePerformIO)
import Text.ParserCombinators.ReadP (char, choice, between, munch1, optional, satisfy, string)
import Text.ParserCombinators.ReadPrec (lift)
import Text.Read (Read(readPrec))

input :: [Instruction Char]
input = map read $ lines $ unsafePerformIO $ readFile "day21.txt"

data Instruction a where
    SwapPositions :: Int -> Int -> Instruction a
    SwapLetters :: a -> a -> Instruction a
    RotateLeft :: Int -> Instruction a
    RotateRight :: Int -> Instruction a
    RotateLetter :: a -> Instruction a
    ReversePositions :: Int -> Int -> Instruction a
    MovePosition :: Int -> Int -> Instruction a

instance Read (Instruction Char) where
    readPrec = lift $ choice
      [ liftM2 SwapPositions (string "swap position " >> read <$> munch1 isDigit)
            (string " with position " >> read <$> munch1 isDigit)
      , liftM2 SwapLetters (string "swap letter " >> satisfy isAlpha)
            (string " with letter " >> satisfy isAlpha)
      , RotateLeft <$> between (string "rotate left ") (string " step" >> optional (char 's'))
            (read <$> munch1 isDigit)
      , RotateRight <$> between (string "rotate right ") (string " step" >> optional (char 's'))
            (read <$> munch1 isDigit)
      , string "rotate based on position of letter " >> RotateLetter <$> satisfy isAlpha
      , liftM2 ReversePositions (string "reverse positions " >> read <$> munch1 isDigit)
            (string " through " >> read <$> munch1 isDigit)
      , liftM2 MovePosition (string "move position " >> read <$> munch1 isDigit)
            (string " to position " >> read <$> munch1 isDigit)
      ]

scramble, unscramble :: (Eq c) => Instruction c -> [c] -> [c]
scramble (SwapPositions x y)
        (splitAt (min x y) -> (begin, a : (splitAt (abs (x - y) - 1) -> (mid, b : end)))) =
    begin ++ b : mid ++ a : end
scramble (SwapLetters a b) s = [if c == a then b else if c == b then a else c | c <- s]
scramble (RotateLeft n) s@(length -> l) = uncurry (flip (++)) $ splitAt (mod n l) s
scramble (RotateRight n) s = scramble (RotateLeft $ negate n) s
scramble (RotateLetter c) s@ ~(elemIndex c -> Just n) =
    scramble (RotateRight $ n + if n < 4 then 1 else 2) s
scramble (ReversePositions x y)
        (splitAt (min x y) -> (begin, splitAt (abs (x - y) + 1) -> (mid, end))) =
    begin ++ reverse mid ++ end
scramble (MovePosition from to) (splitAt from -> (begin, c : end)) =
    uncurry ((. (c :)) . (++)) $ splitAt to $ begin ++ end
unscramble (RotateLeft n) s = scramble (RotateRight n) s
unscramble (RotateRight n) s = scramble (RotateLeft n) s
unscramble (RotateLetter c) s@ ~(elemIndex c -> Just n) = head
    [ scramble (RotateLeft $ n - m) s
    | m <- [0..], (2 * m + if m < 4 then 1 else 2) `mod` length s == n ]
unscramble ins@(MovePosition x y) s = scramble (MovePosition y x) s
unscramble ins s = scramble ins s

main :: IO ()
main = do
    putStrLn $ foldl' (flip scramble) "abcdefgh" input
    putStrLn $ foldr unscramble "fbgdceah" input
