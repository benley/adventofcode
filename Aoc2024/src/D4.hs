module D4 where

import Paths_Aoc2024 (getDataFileName)

look :: [[a]] -> (Int, Int) -> Maybe a
look grid (x, y)
  | x>=0 && y>=0 && x < length grid && y < length (head grid) = Just (grid!!y!!x)
  | otherwise = Nothing

runD4 :: IO ()
runD4 = do
  input <- lines <$> (readFile =<< getDataFileName "day04-input.txt")
  let width = length (head input)
      height = length input
      coords = [(x,y) | x <- [0..(width-1)], y <- [0..(height-1)]]
  putStr "Part 1: "
  print (length $ filter id [find input "XMAS" slope (x, y) | slope <- slopes, (x, y) <- coords])
  putStr "Part 2: "
  print (length $ filter id [checkP2 input (x, y) | (x, y) <- coords])

type Slope = (Int, Int)
type Grid = [[Char]]

slopes :: [Slope]
slopes = [(x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0, 0)]

find :: Grid -> String -> Slope -> (Int, Int) -> Bool
find _ [] _ _ = False
find grid [c] _ (x, y) = look grid (x, y) == Just c
find grid (c:cs) slope@(xd, yd) (x, y)
  | look grid (x, y) == Just c = find grid cs slope (x+xd, y+yd)
  | otherwise = False

checkP2 :: Grid -> (Int, Int) -> Bool
checkP2 grid (x, y) =
  (find grid "MAS" (1,  1) (x-1, y-1) || find grid "SAM" (1,  1) (x-1, y-1))
  &&
  (find grid "MAS" (1, -1) (x-1, y+1) || find grid "SAM" (1, -1) (x-1, y+1))
