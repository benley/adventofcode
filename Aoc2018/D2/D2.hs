#!/usr/bin/env runhaskell

module D2_2018 where

import qualified Data.Map as Map

countChars :: String -> Map.Map Char Int
countChars = foldr (\c m -> Map.insertWith (+) c 1 m) Map.empty

nChars :: Eq a => a -> [Map.Map k a] -> [Map.Map k a]
nChars n cm = filter (\m -> Map.size m > 0) (map (Map.filter (==n)) cm)

main :: IO ()
main = do
  input <- lines <$> getContents
  let charCounts = map countChars input
  let twos = nChars 2 charCounts
  let threes = nChars 3 charCounts
  print $ (length twos) * (length threes)
