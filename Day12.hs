{-# LANGUAGE FlexibleInstances, GADTs, NegativeLiterals, ViewPatterns #-}
module Day12 (main) where

import Control.Applicative (optional)
import Data.Array.Unboxed (IArray, Ix, UArray, (!), (//), listArray, assocs)
import Data.Char (isDigit)
import System.IO.Unsafe (unsafePerformIO)
import Text.ParserCombinators.ReadP (ReadP, (+++), char, choice, munch, skipSpaces, string)
import Text.ParserCombinators.ReadPrec (lift)
import Text.Read (Read(readPrec))

input :: (Num value, Read value) => [Instruction Register value]
input = map read $ lines $ unsafePerformIO $ readFile "day12.txt"

data Register = A | B | C | D deriving (Eq, Ix, Ord)
data Instruction register value where
    Cpy :: Either register value -> register -> Instruction register value
    Inc :: register -> Instruction register value
    Dec :: register -> Instruction register value
    Jnz :: Either register value -> Int -> Instruction register value

instance (Num value, Read value) => Read (Instruction Register value) where
    readPrec = lift $ do
        let int :: (Num a, Read a) => ReadP a
            int = do
                neg <- optional $ char '-'
                num <- read <$> munch isDigit
                return $ maybe num (const $ negate num) neg
            register = choice [A <$ char 'a', B <$ char 'b', C <$ char 'c', D <$ char 'd']
            value = (Left <$> register) +++ (Right <$> int)
        choice
          [ do  string "cpy"; skipSpaces
                val <- value; skipSpaces
                Cpy val <$> register
          , string "inc" >> skipSpaces >> Inc <$> register
          , string "dec" >> skipSpaces >> Dec <$> register
          , do  string "jnz"; skipSpaces
                val <- value; skipSpaces
                Jnz val <$> int
          ]

run :: (IArray a value, Ix register, Eq value, Num value) =>
    [Instruction register value] -> [Instruction register value] -> a register value -> a register value
run _ [] regs = regs
-- This pattern drops the running time from ~10s to ~0s.
run program' (i@(Inc a) : j@(Dec b) : k@(Jnz (Left c) -2) : program) regs
  | a /= b && b == c = run (k:j:i:program') program $ regs // [(a, regs ! a + regs ! b), (b, 0)]
run program' (i@(Cpy value reg) : program) regs =
    run (i:program') program $ regs // [(reg, either (regs !) id value)]
run program' (i@(Inc reg) : program) regs =
    run (i:program') program $ regs // [(reg, regs ! reg + 1)]
run program' (i@(Dec reg) : program) regs =
    run (i:program') program $ regs // [(reg, regs ! reg - 1)]
run program' program@(i@(Jnz val rel) : _) regs
  | 0 <- either (regs !) id val = run (i:program') (tail program) regs
  | rel >= 0, (h, t) <- splitAt rel program = run (reverse h ++ t) t regs
  | (h, t) <- splitAt (- rel) program' = run t (reverse h ++ program) regs

main :: IO ()
main = do
    let program = input; state0 = listArray (A, D) [0, 0, 0, 0] :: UArray Register Int
    print $ run [] program state0 ! A
    print $ run [] program (state0 // [(C, 1)]) ! A
