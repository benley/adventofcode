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

mergeFB :: Char -> Char -> Char
mergeFB '0' _ = '0'
mergeFB '1' _ = '1'
mergeFB '2' x = x
mergeFB  _  _ = error "wat"

showC :: Char -> Char
showC c = case c of
  '0'  -> ' '
  '1'  -> '\x258A'
  '2'  -> '_'
  '\n' -> '\n'
  _    -> error "wat"

main :: IO ()
main = do
  input <- unpack . strip <$> readFile "D8/input.txt"

  let layerlines = splitN (6*25) input
      minZeros = minimumBy orderByZeros layerlines

  putStr "Part 1: "
  print $ length (elemIndices '1' minZeros) * length (elemIndices '2' minZeros)

  putStrLn "Part 2:"
  putStrLn $ map showC $ unlines $ splitN 25 $ foldl1 (zipWith mergeFB) layerlines
