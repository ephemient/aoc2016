{-|
Module      : Common
Description : Common utilities
-}
module Common (readDataFile) where

import Control.Applicative
import Paths_aoc2016 (getDataFileName)

-- |The 'readDataFile' function reads the contents of a file in the data directory as a string.
readDataFile :: String -> IO String
readDataFile fileName = getDataFileName fileName >>= readFile
