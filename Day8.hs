{-# LANGUAGE FlexibleContexts, RecordWildCards, ViewPatterns #-}
module Day8 (main) where

import Data.Array.Unboxed (IArray, Ix, UArray, (!), (//), bounds, elems, listArray, range)
import Data.Char (isDigit)
import Data.List (foldl')
import System.IO.Unsafe (unsafePerformIO)
import Text.ParserCombinators.ReadP (char, choice, munch, skipSpaces, string)
import Text.ParserCombinators.ReadPrec (lift)
import Text.Read (Read(readPrec))

input :: (Read a) => [Command a]
input = map read $ lines $ unsafePerformIO $ readFile "day8.txt"

data Command a = Rect { x :: a, y :: a } | RotRow { y :: a, z :: a } | RotCol { x :: a, z :: a }

instance (Read a) => Read (Command a) where
    readPrec = lift $ choice
      [ do  string "rect"; skipSpaces
            x <- number; char 'x'; y <- number
            return Rect {..}
      , do  string "rotate"; skipSpaces; string "row"; skipSpaces
            string "y="; y <- number; by; z <- number
            return RotRow {..}
      , do  string "rotate"; skipSpaces; string "column"; skipSpaces
            string "x="; x <- number; by; z <- number
            return RotCol {..}
      ]
      where number = read <$> munch isDigit; by = skipSpaces >> string "by" >> skipSpaces

wrap :: (Integral a) => a -> a -> a -> a
wrap l h n = (n - l) `mod` (h - l + 1) + l

run :: (IArray a Bool, Integral z, Ix z) => a (z, z) Bool -> Command z -> a (z, z) Bool
run a@(bounds -> (l, _)) Rect {..} =
    a // [(i, True) | i <- range (l, (y - 1, x - 1))]
run a@(bounds -> ((_, lx), (_, hx))) RotRow {..} =
    a // [((y, wrap lx hx $ x + z), a ! (y, x)) | x <- [lx .. hx]]
run a@(bounds -> ((ly, _), (hy, _))) RotCol {..} =
    a // [((wrap ly hy $ y + z, x), a ! (y, x)) | y <- [ly .. hy]]

display :: (IArray a Bool, Ix x, Ix y) => a (y, x) Bool -> String
display a@(bounds -> ((ly, lx), (hy, hx))) =
    unlines [[c x y | x <- range (lx, hx)] | y <- range (ly, hy)]
  where c x y = if a ! (y, x) then '\x2593' else '\x2591'

main :: IO ()
main = do
    let a0 = listArray ((0, 0), (5, 49)) $ repeat False
        a = foldl' run a0 input :: UArray (Int, Int) Bool
    print $ length $ filter id $ elems a
    putStr $ display a
