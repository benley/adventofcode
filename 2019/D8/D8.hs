module D8_2019 where

import Data.List
import Data.Text (strip, unpack)
import Data.Text.IO (readFile)
import Prelude hiding (readFile)

splitN :: Int -> [a] -> [[a]]
splitN _ [] = []
splitN n xs | length xs >= n = take n xs : splitN n (drop n xs)
            | otherwise = [xs]

orderByZeros :: String -> String -> Ordering
orderByZeros xs ys = compare (length $ elemIndices '0' xs) (length $ elemIndices '0' ys)

main :: IO ()
main = do
  input <- unpack . strip <$> readFile "D8/input.txt"

  putStr "Part 1: "
  let layerlines = map concat (splitN 6 (splitN 25 input))
      minZeros = minimumBy orderByZeros layerlines

  print $ length (elemIndices '1' minZeros) * length (elemIndices '2' minZeros)
