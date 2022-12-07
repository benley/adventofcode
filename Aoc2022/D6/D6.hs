module Main where

import Data.Set as S hiding (drop, take)

findKey :: Ord a => Int -> [a] -> [a]
findKey n xs | length xs >= n && S.size (S.fromList (take n xs)) == n = drop n xs
findKey n (_:xs) = findKey n xs
findKey _ [] = []

main :: IO ()
main = do
  input <- readFile "D6/input.txt"
  putStr "Part 1: " >> print (length input - length (findKey 4 input))
  putStr "Part 2: " >> print (length input - length (findKey 14 input))
