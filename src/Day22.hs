{-|
Module      : Day22
Description : <http://adventofcode.com/2016/day/22 Day 22: Grid Computing>
-}
{-# LANGUAGE BangPatterns, NamedFieldPuns, RecordWildCards, ViewPatterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day22 (main) where

import Control.Monad (guard)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Builder.Extra (intHost)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Short (ShortByteString, toShort)
import Data.Char (isDigit)
import Data.List ((\\), sort)
import Data.Maybe (catMaybes)
import Data.Set (Set, fromList, insert, member, singleton)
import System.IO.Unsafe (unsafePerformIO)

input :: (Num a, Read a) => [(Loc, (a, a))]
input = map (parse . words) $ drop 2 $ lines $ unsafePerformIO $ readFile "day22.txt" where
    parse (n:_:u:a:_) = (loc n, (size u, size a))
    size (reads -> (d, "T"):_) = d
    loc (dropWhile (/= 'x') -> 'x' : (span isDigit -> (x, '-':'y':y))) = (read x, read y)

type Loc = (Int, Int)
data State = State {blank :: [Loc], movable :: Set Loc, datum :: Loc}

initialState :: (Num a, Ord a) => [(Loc, (a, a))] -> State
initialState input = State {..} where
    blank = [p | (p, (0, _)) <- input]
    movable = fromList [p | (p, (u, _)) <- input, any (\(q, (_, a)) -> p /= q && u < a) input]
    datum = (maximum $ fst . fst <$> input, 0)

save :: State -> ShortByteString
save State {blank, datum} = toShort $ toStrict $ toLazyByteString $ mconcat
    [intHost x `mappend` intHost y | (x, y) <- datum : sort blank]

step :: (Enum a) => (State, a) -> Set ShortByteString -> ([(State, a)], Set ShortByteString)
step (state@State {..}, !k) visited = (next, visited') where
    (catMaybes -> next, visited':_) = unzip $ scanr add (Nothing, visited) $ do
        p@(x, y) <- blank
        q <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
        guard $ member q movable
        return state {blank = q : (blank \\ [p]), datum = if q == datum then p else datum}
    add state@(save -> save') (_, visited)
      | member save' visited = (Nothing, visited)
      | otherwise = (Just (state, succ k), insert save' visited)

unfold :: (a -> b -> ([a], b)) -> b -> [a] -> [a]
unfold _ _ [] = []
unfold f k as = as ++ unfold f k' as' where
    unfold' k [] = ([], k)
    unfold' k (a:as) = let (as', k') = f a k; ~(as'', k'') = unfold' k' as in (as' ++ as'', k'')
    (as', k') = unfold' k as

main :: IO ()
main = do
    print $ length [() | (p, (u, _)) <- input, u /= 0, (q, (_, a)) <- input, p /= q, u <= a]
    let state0 = initialState input
        done (State {datum = (0, 0)}, _) = True
        done _ = False
    print $ snd $ head $ filter done $ unfold step (singleton $ save state0) [(state0, 0)]
