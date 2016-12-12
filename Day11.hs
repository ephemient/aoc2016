{-# LANGUAGE BangPatterns, FlexibleContexts, GADTs, ParallelListComp, TupleSections, ViewPatterns #-}
module Day11 (main) where

import Control.Applicative ((<$))
import Control.Monad (guard, liftM2)
import Data.Array.Unboxed (UArray, IArray, Ix, (!), (//), assocs, bounds, listArray, range)
import Data.Bits ((.|.), finiteBitSize)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Builder.Extra (intHost)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Short (ShortByteString, toShort)
import Data.Char (isAlphaNum, toUpper)
import Data.Function (on)
import Data.List (elemIndex, inits, nub, nubBy, sortBy, tails, transpose)
import Data.Maybe (catMaybes, maybeToList)
import Data.Ord (comparing)
import Data.Set (Set, insert, member, singleton, size)
import Text.ParserCombinators.ReadP (ReadP, (+++), between, choice, many1, munch, readP_to_S, sepBy1, string)
import System.IO.Unsafe (unsafePerformIO)

input :: [[(String, Danger)]]
input = do
    let content = lines $ unsafePerformIO $ readFile "day11.txt"
    parses <- readP_to_S parse <$> content
    let (items, _):_ = filter (null . snd) parses
    return items

data Floor = First | Second | Third | Fourth deriving (Bounded, Enum, Eq, Ix, Ord)
data Danger = Microchip | Generator deriving (Bounded, Eq, Ix, Ord)
data State array element where
    State :: (IArray array Bool, Ix element) =>
        Floor -> array (Floor, element, Danger) Bool -> State array element

parse :: ReadP [(String, Danger)]
parse = do
    string "The "; choice $ map string ["first", "second", "third", "fourth"]
    string " floor contains " `between` string "." $
        sepBy1 item (string " and " +++ string ", " +++ string ", and ") +++
        ([] <$ string "nothing relevant")
 where
   item = do
        element <- string "a " >> munch isAlphaNum
        danger <- (Generator <$ string " generator") +++
            (Microchip <$ string "-compatible microchip")
        return (element, danger)

initialState :: (IArray array Bool) => [[(String, Danger)]] -> State array Int
initialState input' = State minBound $ listArray bounds (repeat False) // placements
  where
    elements = nub (input >>= map fst) ++ ["elerium", "dilithium"]
    bounds = ((minBound, 0, minBound), (maxBound, length elements - 1, maxBound))
    placements = do
        (floor, items) <- zip [minBound ..] input'
        ((`elemIndex` elements) -> Just element, danger) <- items
        return ((floor, element, danger), True)

showState :: (IArray array Bool) => State array Int -> String
showState (State elevator things) = unlines $ do
    floor <- reverse $ range (minFloor, maxFloor)
    let showFloor = show $ fromEnum floor + 1
        showElevator = if elevator == floor then "E" else " "
        showElement = (!!) elementNames
        showDanger Microchip = 'M'
        showDanger Generator = 'G'
        showItem (element, danger) | things ! (floor, element, danger) = showDanger danger : showElement element
        showItem _ = replicate (elementNameLength + 1) ' '
    return $ unwords $ showFloor : showElevator : map showItem (range ((minElement, minDanger), (maxElement, maxDanger)))
  where
    elements = map (map toUpper) $ nub (input >>= map fst) ++ ["elerium", "dilithium"]
    (elementNameLength, elementNames) : _ = dropWhile ((< length elements) . length . nub . snd) $ zip [0..] $ map transpose $ inits $ transpose $ map (++ repeat ' ') elements
    ((minFloor, minElement, minDanger), (maxFloor, maxElement, maxDanger)) = bounds things

save :: (IArray array Bool, Ix element) => State array element -> ShortByteString
save (State elevator things) = toShort $ toStrict $ toLazyByteString $
    foldr (mappend . intHost) (intHost $ fromEnum elevator) $ toInts flattened
  where
    ((minFloor, minElement, minDanger), (maxFloor, maxElement, maxDanger)) = bounds things
    occurrences = nubBy ((==) `on` fst)
      [ (element, (floor, danger))
      | ((floor, element, danger), True) <- assocs things ]
    elements = sortBy (comparing (`lookup` occurrences)) $ range (minElement, maxElement)
    flattened = do
        (floor, danger) <- range ((minFloor, minDanger), (maxFloor, maxDanger))
        map ((!) things . (floor,, danger)) elements
    toInts [] = []
    toInts (splitAt (finiteBitSize (0 :: Int)) -> (h, t)) =
        foldr (.|.) 0 [if b then i else 0 | b <- h | i <- iterate (* 2) 1] : toInts t

moves :: (Enum depth, IArray array Bool, Ix element) =>
    ([State array element], State array element, depth) -> Set ShortByteString ->
    ([([State array element], State array element, depth)], Set ShortByteString)
moves (path, s@(State elevator things), !k) visited = (map (s:path,, succ k) states, visited') where
    ((minFloor, minElement, Microchip), (maxFloor, maxElement, Generator)) = bounds things
    toFloor floor (element, danger) = (floor, element, danger)
    safe things floor = let elements = range (minElement, maxElement) in
        not (any ((!) things . (floor,, Generator)) elements) ||
        all ((!) things . (floor,, Generator)) (filter ((!) things . (floor,, Microchip)) elements)
    next = do
        elevator' <- (if elevator <= minFloor then [] else [pred elevator]) ++
                     (if elevator >= maxFloor then [] else [succ elevator])
        item1:items' <- tails $ range ((minElement, minBound), (maxElement, maxBound))
        guard $ things ! toFloor elevator item1
        item2 <- Nothing : map Just items'
        guard $ maybe True ((!) things . toFloor elevator) item2
        let items = item1 : maybeToList item2
            things' = things //
              [ (toFloor floor item, on) | item <- items
              , (floor, on) <- [(elevator, False), (elevator', True)] ]
        guard $ safe things' elevator && safe things' elevator'
        return (compare elevator' elevator, length items, State elevator' things')
    (catMaybes -> states, visited':_) = unzip $ scanr add (Nothing, visited)
      [ state
      | (dir, count, state) <- next
        -- This is the "Kind of important" optimization from
        -- https://www.reddit.com/r/adventofcode/comments/5hoia9/2016_day_11_solutions/db1v1ws/
        -- I am not confident that this is always a valid optimization, but
        -- it brings the running time down from ~30s to ~2s.
      , dir /= LT || count < 2 || not (any (\(dir, count, _) -> dir == LT && count < 2) next)
      , dir /= GT || count > 1 || not (any (\(dir, count, _) -> dir == GT && count > 1) next)
      ]
    add state@(save -> save') (_, visited)
      | member save' visited = (Nothing, visited)
      | otherwise = (Just state, insert save' visited)

unfold :: (a -> b -> ([a], b)) -> b -> [a] -> [a]
unfold _ _ [] = []
unfold f k as = as' ++ unfold f k' as' where
    unfold' k [] = ([], k)
    unfold' k (a:as) = let (as', k') = f a k; ~(as'', k'') = unfold' k' as in (as' ++ as'', k'')
    (as', k') = unfold' k as

main :: IO ()
main = do
    let expand state0 = unfold moves (singleton $ save state0) [([], state0, 0)]
        states1 = expand $ initialState input :: [([State UArray Int], State UArray Int, Int)]
        states2 = expand $ initialState $
            (head input ++ liftM2 (,) ["elerium", "dilithium"] [Microchip, Generator]) :
            tail input :: [([State UArray Int], State UArray Int, Int)]
        done (_, State _ things, _) = not $ or
          [ things ! (floor, element, danger)
          | (floor, element, danger) <- range $ bounds things
          , floor /= maxBound ]
        (path1, state1, distance1) : _ = filter done states1
        (path2, state2, distance2) : _ = filter done states2
    print distance1; putStrLn ""; mapM_ (putStrLn . showState) $ reverse $ state1 : path1
    print distance2; putStrLn ""; mapM_ (putStrLn . showState) $ reverse $ state2 : path2
