module Main where

import Data.List (sort)
import qualified Data.Set as Set

main = do
  nums <- sort . map read . lines <$> readFile "Aoc2020/input1.txt"
  let checkN n = n < 1000 && (2020 - n) `Set.member` Set.fromAscList nums
      firstMatch = head $ filter checkN nums
  print (firstMatch * (2020 - firstMatch))
