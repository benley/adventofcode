-- | https://adventofcode.com/2022/day/3

module Main (main) where

import Data.List (foldl')
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Maybe (fromJust)

compartments :: String -> (String, String)
compartments x = splitAt (length x `div` 2) x

priority :: Char -> Int
priority x = fromJust $ M.lookup x priorities
  where priorities = M.fromList (zip (['a'..'z'] ++ ['A'..'Z']) [1..])

common :: (String, String) -> Char
common (a, b) = head $ S.toList $ S.intersection (S.fromList a) (S.fromList b)

main :: IO ()
main = do
  input <- lines <$> readFile "D3/input.txt"
  putStr "Part 1: "
  print $ sum (map (priority . common . compartments) input)

  putStr "Part 2: "
  print $ sum (map (priority . commonN) (group 3 input))

commonN :: [String] -> Char
commonN = head . S.toList . intersectN . map S.fromList

intersectN :: Ord a => [S.Set a] -> S.Set a
intersectN (x:xs) = Data.List.foldl' S.intersection x xs
intersectN [] = S.empty

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l = take n l : group n (drop n l)
