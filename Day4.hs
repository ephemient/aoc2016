{-# LANGUAGE RecordWildCards #-}
module Day4 (main) where

import Data.Char (isAlpha, isDigit)
import Data.List (elemIndex, find, group, sort, sortBy)
import Data.Maybe (fromJust)
import Data.Ord (Down(Down), comparing)
import Text.ParserCombinators.ReadP (between, char, munch, sepBy)
import Text.ParserCombinators.ReadPrec (lift)
import Text.Read (Read(readPrec))
import System.IO.Unsafe (unsafePerformIO)

input :: [Room]
input = map read $ lines $ unsafePerformIO $ readFile "day4.txt"

data Room = Room { name :: [String], sectorId :: Int, checksum :: String }

instance Read Room where
    readPrec = lift $ do
        name <- munch isAlpha `sepBy` char '-' <* char '-'
        sectorId <- read <$> munch isDigit
        checksum <- char '[' `between` char ']' $ munch isAlpha
        return Room {..}

calculate :: Room -> String
calculate = map head . take 5 . sortBy (comparing $ Down . length) . group . sort . concat . name

decrypt :: Room -> [String]
decrypt Room {..} = map cipher name
  where alphabet = ['a' .. 'z']
        cipher s = [maybe c rotate $ elemIndex c alphabet | c <- s]
        rotate i = alphabet !! ((i + sectorId) `mod` length alphabet)

isRealRoom :: Room -> Bool
isRealRoom room = calculate room == checksum room

main :: IO ()
main = do
    print $ sum $ map sectorId $ filter isRealRoom input
    print $ sectorId $ fromJust $ find (elem "northpole" . decrypt) input
