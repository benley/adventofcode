#!/usr/bin/env runhaskell

module D2 where

import Data.List
import Data.Maybe

part1 :: IO Int
part1 = do
  input <- readFile "input1.txt"
  return $ sum $ map (cksumRow . map readInt . words) (lines input)
    where cksumRow a = maximum a - minimum a

readInt :: String -> Int
readInt = read

part2 :: IO Int
part2 = do
  input <- readFile "input1.txt"
  return $ sum . map (fromMaybe 0) $
    map (finddiv . sortBy (flip compare) . map readInt . words) $
      lines input

finddiv :: [Int] -> Maybe Int
finddiv (a:b:xs)
  | a `mod` b == 0 = Just (a `div` b)
  | otherwise = case finddiv (a:xs) of
                  Nothing -> finddiv (b:xs)
                  j -> j
finddiv [_] = Nothing
finddiv [] = Nothing
