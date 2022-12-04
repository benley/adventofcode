-- | https://adventofcode.com/2022/day/1

module Main where

import Data.List (sort)
import Data.Text (splitOn, strip, pack, unpack)

main :: IO ()
main = do
  input <- fmap (splitOn "\n") . splitOn "\n\n" . strip . pack <$> readFile "D1/input.txt"
  let sums = (reverse . sort) (map (sum . map (read . unpack)) input :: [Int])
  putStrLn ("Part 1: " ++ (show . head) sums)
  putStrLn ("Part 2: " ++ (show . sum . take 3) sums)
