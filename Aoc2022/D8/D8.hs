-- |

module Main (main) where

import Data.List (foldl')
import Data.List.Split (chunksOf)
import Data.Set qualified as Set

type IdTree = (Int, Int)

row :: Int -> [a] -> a
row n = (!! n)

col :: Int -> [[a]] -> [a]
col n = map (row n)

digit :: Char -> Int
digit c = read [c]

visible :: [IdTree] -> [IdTree]
visible =
  foldl' (\acc x -> case acc of
            [] -> [x]
            _  -> if snd x > snd (head acc) then x:acc else acc) []

main :: IO ()
main = do
  grid <- map (map digit) . lines <$> readFile "D8/input.txt"
  let
    width = length (head grid)

    -- give each tree a unique ID
    idGrid :: [[IdTree]]
    idGrid = chunksOf width $ zip [0..] $ concat grid

    visibleByRow = concat $ map visible idGrid ++ map (visible . reverse) idGrid
    inverted = [col n idGrid | n <- [0..width - 1]]
    visibleByCol = concatMap visible inverted ++ concatMap (visible . reverse) inverted
    result = Set.fromList (visibleByRow ++ visibleByCol)

  print (length result)
