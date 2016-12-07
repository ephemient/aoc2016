module Day7 (main) where

import Control.Monad (guard, replicateM)
import Data.Char (isAlphaNum)
import Data.Set (empty, insert, member)
import System.IO.Unsafe (unsafePerformIO)
import Text.ParserCombinators.ReadP (ReadP, (<++), between, char, eof, look, many, munch, readP_to_S, satisfy)

input :: [String]
input = lines $ unsafePerformIO $ readFile "day7.txt"

abba :: ReadP Bool
abba = (<* munch isAlphaNum) $ (<++ return False) $ True <$ do
    many $ satisfy isAlphaNum
    a:b:c:d:_ <- replicateM 4 $ satisfy isAlphaNum
    guard $ a /= b && b == c && a == d

tls :: ReadP Bool
tls = tls' False where
    tls' k = do
        super <- abba
        let k' = k || super
        (<++ (k' <$ eof)) $ do
        hyper <- char '[' `between` char ']' $ abba
        if hyper then return False else tls' k'

ssl :: ReadP Bool
ssl = ssl' empty empty where
    ssl' j k = (<++ (char '[' <++ char ']' >> ssl' k j) <++ (False <$ eof)) $ do
        a <- satisfy isAlphaNum
        (<++ ssl' j k) $ do
            b:c:_ <- look
            guard $ isAlphaNum b && a /= b && a == c
            if member (b, a) j then return True else ssl' j $ insert (a, b) k

matches :: ReadP Bool -> String -> Bool
matches p = any fst . readP_to_S p

main :: IO ()
main = do
    print $ length $ filter (matches tls) input
    print $ length $ filter (matches ssl) input
