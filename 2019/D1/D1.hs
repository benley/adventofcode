module D1_2019 where

main :: IO ()
main = do
  input' <- lines <$> readFile "D1/input.txt"
  print $ sum [simpleFuel (read x) | x <- input']
  print $ sum [fancyFuel (read x) | x <- input']

simpleFuel :: Int -> Int
simpleFuel n = n `div` 3 - 2

fancyFuel :: Int -> Int
fancyFuel n
  | n <= 0 = 0
  | otherwise = do
      let n' = max 0 (simpleFuel n)
      n' + max 0 (fancyFuel n')
