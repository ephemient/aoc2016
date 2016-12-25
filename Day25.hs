{-# LANGUAGE FlexibleInstances, GADTs, NegativeLiterals #-}
module Day25 (main) where

import Control.Applicative (optional)
import Control.Monad (liftM2)
import Data.Char (isDigit)
import System.IO.Unsafe (unsafePerformIO)
import Text.ParserCombinators.ReadP (ReadP, (+++), char, choice, munch1, skipSpaces, string)
import Text.ParserCombinators.ReadPrec (lift)
import Text.Read (Read(readPrec))

input :: (Num value, Read value) => [Instruction Register value]
input = map read $ lines $ unsafePerformIO $ readFile "day25.txt"

data Register = A | B | C | D
data Instruction register value where
    Cpy :: Either register value -> Either register value -> Instruction register value
    Inc :: Either register value -> Instruction register value
    Dec :: Either register value -> Instruction register value
    Jnz :: Either register value -> Either register value -> Instruction register value
    Out :: Either register value -> Instruction register value

instance (Num value, Read value) => Read (Instruction Register value) where
    readPrec = lift $ do
        let int = read <$> liftM2 (maybe id (:)) (optional $ char '-') (munch1 isDigit)
            register = choice [A <$ char 'a', B <$ char 'b', C <$ char 'c', D <$ char 'd']
            operand = (Left <$> register) +++ (Right <$> int)
        choice
          [ string "cpy" >> skipSpaces >> liftM2 Cpy operand (skipSpaces >> Left <$> register)
          , string "inc" >> skipSpaces >> Inc . Left <$> register
          , string "dec" >> skipSpaces >> Dec . Left <$> register
          , string "jnz" >> skipSpaces >> liftM2 Jnz operand (skipSpaces >> operand)
          , string "out" >> skipSpaces >> Out <$> operand
          ]

main :: IO ()
main = do
    let [Cpy (Left A) (Left D),
         Cpy (Right c1) (Left C),
         Cpy (Right c2) (Left B),
         Inc (Left D),
         Dec (Left B),
         Jnz (Left B) (Right -2),
         Dec (Left C),
         Jnz (Left C) (Right -5),
         Cpy (Left D) (Left A),
         Jnz (Right 0) (Right 0),
         Cpy (Left A) (Left B),
         Cpy (Right 0) (Left A),
         Cpy (Right 2) (Left C),
         Jnz (Left B) (Right 2),
         Jnz (Right 1) (Right 6),
         Dec (Left B),
         Dec (Left C),
         Jnz (Left C) (Right -4),
         Inc (Left A),
         Jnz (Right 1) (Right -7),
         Cpy (Right 2) (Left B),
         Jnz (Left C) (Right 2),
         Jnz (Right 1) (Right 4),
         Dec (Left B),
         Dec (Left C),
         Jnz (Right 1) (Right -4),
         Jnz (Right 0) (Right 0),
         Out (Left B),
         Jnz (Left A) (Right -19),
         Jnz (Right 1) (Right -21)] = input
    print $ head $ dropWhile (< 0) $ map (subtract $ c1 * c2) $ iterate ((+ 2) . (* 4)) 2
