{-|
Module      : Day23
Description : <http://adventofcode.com/2016/day/23 Day 23: Safe-Cracking>
-}
{-# LANGUAGE FlexibleInstances, GADTs, NegativeLiterals, ViewPatterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day23 (main) where

import Common (readDataFile)
import Control.Applicative (optional)
import Control.Monad (liftM2)
import Data.Array.Unboxed (IArray, Ix, UArray, (!), (//), listArray)
import Data.Char (isDigit)
import System.IO.Unsafe (unsafePerformIO)
import Text.ParserCombinators.ReadP (ReadP, (+++), char, choice, munch1, skipSpaces, string)
import Text.ParserCombinators.ReadPrec (lift)
import Text.Read (Read(readPrec))

input :: (Num value, Read value) => [Instruction Register value]
input = map read $ lines $ unsafePerformIO $ readDataFile "day23.txt"

data Register = A | B | C | D deriving (Eq, Ix, Ord)
data Instruction register value where
    Cpy :: Either register value -> Either register value -> Instruction register value
    Inc :: Either register value -> Instruction register value
    Dec :: Either register value -> Instruction register value
    Jnz :: Either register value -> Either register value -> Instruction register value
    Tgl :: Either register value -> Instruction register value

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
          , string "tgl" >> skipSpaces >> Tgl <$> operand
          ]

run :: (IArray a value, Ix register, Integral value) => [Instruction register value] ->
    [Instruction register value] -> a register value -> a register value
run _ [] regs = regs
-- Peephole optimization: [cpy b c, inc a, dec c, jnz c -2, dec d, jnz d -2] = [a += c * d, c = d = 0]
run program' (i@(Cpy a (Left b)) : j@(Inc (Left c)) : k@(Dec (Left d)) :
        l@(Jnz (Left e) (Right -2)) : m@(Dec (Left f)) : n@(Jnz (Left g) (Right -5)) : program) regs
  | either (\a -> a /= b && a /= c && a /= d && a /= f) (const True) a
  , b /= c, b == d, b /= f, c /= f, d == e, f == g =
    run (n:m:l:k:j:i:program') program $
    regs // [(c, regs ! c + either (regs !) id a * regs ! f), (d, 0), (f, 0)]
-- Peephole optimization: [inc a, dec b, jnz b -2] = [a += b, b = 0]
run program' (i@(Inc (Left a)) : j@(Dec (Left b)) : k@(Jnz (Left c) (Right -2)) : program) regs
  | a /= b, b == c = run (k:j:i:program') program $ regs // [(a, regs ! a + regs ! b), (b, 0)]
-- Peephole optimization: [dec a, inc b, jnz a -2] = [b += a, a = 0]
run program' (i@(Dec (Left a)) : j@(Inc (Left b)) : k@(Jnz (Left c) (Right -2)) : program) regs
  | a /= b, a == c = run (k:j:i:program') program $ regs // [(a, 0), (b, regs ! a + regs ! b)]
run program' (i@(Cpy value (Left reg)) : program) regs =
    run (i:program') program $ regs // [(reg, either (regs !) id value)]
run program' (i@(Inc (Left reg)) : program) regs =
    run (i:program') program $ regs // [(reg, regs ! reg + 1)]
run program' (i@(Dec (Left reg)) : program) regs =
    run (i:program') program $ regs // [(reg, regs ! reg - 1)]
run program' program@(i@(Jnz value rel) : _) regs
  | 0 <- either (regs !) id value = run (i:program') (tail program) regs
  | rel' >= 0, (h, t) <- splitAt rel' program = run (reverse h ++ t) t regs
  | (h, t) <- splitAt (- rel') program' = run t (reverse h ++ program) regs
  where rel' = fromIntegral $ either (regs !) id rel
run program' program@(i@(Tgl rel) : _) regs
  | rel' >= 0, (i':program'') <- toggle rel' program = run (i':program') program'' regs
  | rel' < 0 = run (toggle (-rel') $ i:program') (tail program) regs
  | otherwise = run (i:program') (tail program) regs
  where rel' = fromIntegral $ either (regs !) id rel
-- Skip invalid instructions
run program' (i:program) regs = run (i:program') program regs

toggle :: Int -> [Instruction register value] -> [Instruction register value]
toggle n (splitAt n -> (h, i:t)) = h ++ i' : t where
    i' = case i of
        Cpy a b -> Jnz a b
        Jnz a b -> Cpy a b
        Inc a -> Dec a
        Dec a -> Inc a
        Tgl a -> Inc a
-- Skip if out of bounds
toggle _ program = program

main :: IO ()
main = do
    print $ run [] input (listArray (A, D) [7] :: UArray Register Int) ! A
    print $ run [] input (listArray (A, D) [12] :: UArray Register Int) ! A
