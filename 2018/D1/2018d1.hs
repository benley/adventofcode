#!/usr/bin/env runhaskell

module D1_2018 where

import qualified Data.Set as Set

parseLine :: String -> Int
parseLine ('+':num) = read num
parseLine ('-':num) = -1 * read num
parseLine n = error "wtf"

firstDoubleSum :: [Int] -> Int -> Set.Set Int -> Int
firstDoubleSum (change:ns) current seen =
  let result = current + change in
    if result `Set.member` seen then result
    else firstDoubleSum ns result (Set.insert result seen)

main2 nums = firstDoubleSum (cycle nums) 0 (Set.fromList [0])

main = do
  nums <- fmap parseLine . lines <$> getContents
  putStrLn ("Part 1: " ++ show (sum nums))
  putStrLn ("Part 2: " ++ show (main2 nums))
