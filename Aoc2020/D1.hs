{-# LANGUAGE TupleSections #-}

module Main where

import Data.List (sort)
import qualified Data.Set as Set

part1 = do
  nums <- sort . map read . lines <$> readFile "Aoc2020/input1.txt"
  let checkN n = n < 1000 && (2020 - n) `Set.member` Set.fromAscList nums
      firstMatch = head $ filter checkN nums
  print (firstMatch * (2020 - firstMatch))


-- pairs :: [a] -> [(a, a)]
pairs (x1:xs) = map (x1,) xs ++ pairs xs
pairs _ = []

getNums :: IO [Int]
getNums = sort . map read . lines <$> readFile "Aoc2020/input1.txt"

asdf ((n1, n2) : ns) (x : xs) =

part2 = do
  ps <- pairs <$> getNums
  let ps_ = filter ( \(x, y) -> x+y < 2020 ) ps
      -- asdf :: (Int, Int) -> [Int]
      -- asdf (n1, n2) = filter (\n3 -> (n1+n2+n3) == 2020)

  print $ length ps
  print $ length ps_


main = do
  part1
  part2
