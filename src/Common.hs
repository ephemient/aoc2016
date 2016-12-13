{-|
Module      : Common
Description : Common utilities
-}
module Common (readDataFile, unfold) where

import Control.Applicative
import Paths_aoc2016 (getDataFileName)

-- |The 'readDataFile' function reads the contents of a file in the data directory as a string.
readDataFile :: String -> IO String
readDataFile fileName = getDataFileName fileName >>= readFile

-- |The 'unfold' function is like 'Data.List.unfoldr' generalized to a tree, or
-- like an accumulating breadth-first traversal of 'Data.Tree.unfoldTree'.
--
-- In the special case where each node of the tree has at most one child,
-- 'unfold' will build the same list that 'Data.List.unfoldr' would.
--
-- prop> unfoldr f b0 == unfold (\_ b -> maybe ([], b) (\(a, b') -> ([a], b')) (f b)) b0 [undefined]
--
-- In the special case where there is no accumulator being passed through every
-- level built, 'unfold' will produce the same list that a breadth-first
-- traversal of 'Data.Tree.unfoldTree' would.
--
-- prop> concat (levels (foldTree f b0)) == map fst (unfold (\(_, bs) _ -> (map f bs, ())) () [f b0])
unfold ::
    (a -> b -> ([a], b)) -- ^A function from an element and accumulator to a
    -- tuple of list of children elements and updated accumulator
    -> b -- ^The initial accumulator
    -> [a] -- ^The initial set of elements
    -> [a] -- ^The results
unfold _ _ [] = []
unfold f k as = as ++ unfold f k' as' where
    unfold' k [] = ([], k)
    unfold' k (a:as) = let (as', k') = f a k; ~(as'', k'') = unfold' k' as in (as' ++ as'', k'')
    (as', k') = unfold' k as
