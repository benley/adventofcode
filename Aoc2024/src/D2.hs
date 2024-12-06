module D2 where

import Data.List (sort)
import Data.Text (lines, words, Text)
import Data.Text.IO (readFile)
import Data.Text.Read (decimal)
import Paths_Aoc2024 (getDataFileName)
import Prelude hiding (lines, readFile, words)
import Data.Either (rights)

parse :: Text -> [Int]
parse line = fst <$> rights (decimal <$> words line)

isSafe :: [Int] -> Bool
isSafe xs = deltas xs && (allIncreasing || allDecreasing)
  where
    allIncreasing = sort xs == xs
    allDecreasing = sort xs == reverse xs
    deltas (n1:n2:ns) = let d = abs (n2 - n1) in d > 0 && d <= 3 && deltas (n2:ns)
    deltas [_] = True
    deltas [] = True

options :: [a] -> [[a]]
options xs = [ take n xs ++ drop (n+1) xs | n <- [0..length xs] ]

isSafe2 :: [Int] -> Bool
isSafe2 xs = any isSafe (options xs)

runD2 :: IO ()
runD2 = do
  input <- map parse . lines <$> (readFile =<< getDataFileName "day02-input.txt")
  putStr "Part 1: "
  print (length (filter isSafe input))
  putStr "Part 2: "
  print (length (filter isSafe2 input))
