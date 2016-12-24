module Day24 (main) where

import Control.Monad (guard)
import Data.Array (Array, (!), array, bounds, inRange)
import Data.Char (isDigit)
import Data.List ((\\), permutations, tails)
import Data.Maybe (catMaybes)
import qualified Data.Map as M (Map, (!), fromList)
import qualified Data.Set as S (delete, fromList, insert, member, null, singleton)
import System.IO.Unsafe (unsafePerformIO)

input :: [String]
input = lines $ unsafePerformIO $ readFile "day24.txt"

layout :: Array (Int, Int) Bool
layout = array ((0, 0), (length input - 1, maximum (map length input) - 1))
    [((y, x), c /= '#') | (y, line) <- zip [0..] input, (x, c) <- zip [0..] line]

start :: (Int, Int)
start = head [(y, x) | (y, line) <- zip [0..] input, (x, '0') <- zip [0..] line]

wanted :: [(Int, Int)]
wanted = [(y, x) | (y, line) <- zip [0..] input, (x, c) <- zip [0..] line, isDigit c]

pairDistances :: M.Map ((Int, Int), (Int, Int)) Int
pairDistances = M.fromList $ do
    a:rest <- tails wanted
    let step ((y, x), n) visited = (catMaybes next, visited') where
            (next, visited':_) = unzip $ scanr add (Nothing, visited) $ do
                (y', x') <- [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]
                guard $ inRange (bounds layout) (y', x') && layout ! (y', x')
                return (y', x')
            add p (_, visited)
              | S.member p visited = (Nothing, visited)
              | otherwise = (Just (p, succ n), S.insert p visited)
        gather k remain ~((b, n):bs)
          | S.null remain = k
          | S.member b remain = gather (((a, b), n) : ((b, a), n) : k) (S.delete b remain) bs
          | otherwise = gather k remain bs
    gather [] (S.fromList rest) $ unfold step (S.singleton a) [(a, 0)]

walkDistance :: [(Int, Int)] -> Int
walkDistance path = sum [pairDistances M.! pair | pair <- zip path $ tail path]

unfold :: (a -> b -> ([a], b)) -> b -> [a] -> [a]
unfold _ _ [] = []
unfold f k as = as ++ unfold f k' as' where
    unfold' k [] = ([], k)
    unfold' k (a:as) = let (as', k') = f a k; ~(as'', k'') = unfold' k' as in (as' ++ as'', k'')
    (as', k') = unfold' k as

main :: IO ()
main = do
    let orderings = (:) start <$> permutations (wanted \\ [start])
    print $ minimum $ map walkDistance orderings
    print $ minimum $ map walkDistance $ (++ [start]) <$> orderings
