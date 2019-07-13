module Lib
  ( outputGrid
  , formatGrid
  , findWord
  , findWords
  , findWordInLine
  , skew
  ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)

type Grid = [String]

outputGrid :: Grid -> IO ()
outputGrid grid = putStrLn $ formatGrid grid

getLines :: Grid -> [String]
getLines grid =
  let horizontal = grid
      vertical = transpose grid
      diagonal1 = diagonalise grid
      diagonal2 = diagonalise (map reverse grid)
      lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
   in lines ++ (map reverse lines)

diagonalise :: Grid -> Grid
diagonalise = transpose . skew

skew :: Grid -> Grid
skew [] = []
skew (l:ls) = l : map indent (skew ls)
  where
    indent line = '_' : line

findWord :: Grid -> String -> Maybe String
findWord grid word =
  let lines = getLines grid
      found = or $ map (findWordInLine word) lines
   in if found
        then Just word
        else Nothing

findWords :: Grid -> [String] -> [String]
findWords grid words =
  let foundWords = map (findWord grid) words
   in catMaybes foundWords

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf

formatGrid :: Grid -> String
formatGrid = unlines
