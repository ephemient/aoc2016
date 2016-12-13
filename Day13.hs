{-# LANGUAGE ParallelListComp, TupleSections, ViewPatterns #-}
module Day13 (main) where

import Codec.Picture (Image(imageHeight, imageWidth), GifLooping(LoopingForever), PixelRGB8(PixelRGB8), generateImage, pixelAt, writeGifAnimation)
import Data.Array ((//), (!), listArray, range)
import Data.Bits (Bits, popCount, testBit)
import Data.List hiding (insert)
import Data.Set (Set, insert, member, singleton)
import System.IO.Unsafe (unsafePerformIO)

input :: (Num a, Read a) => a
input = read $ unsafePerformIO $ readFile "day13.txt"

data Cell = Wall | Empty | Visited Int | Path Int | Head Int

wall :: (Bits a, Num a) => a -> (a, a) -> Bool
wall input (x, y) = popCount (x^2 + 3*x + 2*x*y + y + y^2 + input) `testBit` 0

walk :: (Bits a, Num a, Ord a) =>
    a -> ((a, a), [(a, a)]) -> Set (a, a) -> ([((a, a), [(a, a)])], Set (a, a))
walk input ((x, y), n) visited = (map (, (x, y) : n) next, foldr insert visited next) where
    next = filter ok [(x + 1, y), (x, y - 1), (x - 1, y), (x, y + 1)]
    ok (x, y) = x >= 0 && y >= 0 && not (wall input (x, y) || member (x, y) visited)

unfold :: (a -> b -> ([a], b)) -> b -> [a] -> [a]
unfold _ _ [] = []
unfold f k as = as ++ unfold f k' as' where
    unfold' k [] = ([], k)
    unfold' k (a:as) = let (as', k') = f a k; ~(as'', k'') = unfold' k' as in (as' ++ as'', k'')
    (as', k') = unfold' k as

main :: IO ()
main = do
    let part1 = (31, 39); part2 = 50
        reach = unfold (walk input) (singleton (1, 1)) [((1, 1), [])] :: [((Int, Int), [(Int, Int)])]
        (before, found:after) = break ((==) part1 . fst) reach
        visited = before ++ found : takeWhile ((<= part2) . length . snd) after
        minMaxTuple ((minX, minY), (maxX, maxY)) (x, y) = ((min minX x, min minY y), (max maxX x, max maxY y))
        (minBound@(x0, y0), maxBound@(x1, y1)) = foldl' minMaxTuple ((0, 0), (0, 0)) $ map fst visited
        maze0 = listArray (minBound, maxBound) [if wall input (x, y) then Wall else Empty | (x, y) <- range (minBound, maxBound)]
        accum maze (p, path@(length -> n)) = (maze // [(p, Visited n)], maze // updates) where
            updates = (p, Head n) : [(p, Path n) | p <- reverse path | n <- [0..]]
        (_, mazes) = mapAccumL accum maze0 visited
        pxCell maze p
          | p == part1, Empty <- maze ! p = PixelRGB8 255 0 0
          | p == part1 = PixelRGB8 0 255 0
          | otherwise =
            case maze ! p of
                Wall -> PixelRGB8 0 0 0
                Empty -> PixelRGB8 255 255 255
                Visited n | n <= part2 -> PixelRGB8 102 102 102 | otherwise -> PixelRGB8 153 153 153
                Path n | n <= part2 -> PixelRGB8 51 51 255 | otherwise -> PixelRGB8 102 102 255
                Head _ -> PixelRGB8 0 0 255
        inflate factor image = generateImage at (factor * imageWidth image) (factor * imageHeight image)
          where at x y = pixelAt image (x `div` factor) (y `div` factor)
    print $ length $ snd found
    print $ length $ takeWhile ((<= part2) . length . snd) visited
    either error id $ writeGifAnimation "day13.gif" 2 LoopingForever
      [ inflate 16 $ generateImage (curry $ pxCell maze) (x1 - x0 + 1) (y1 - y0 + 1) | maze <- mazes]
