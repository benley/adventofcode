-- |

module Main (main) where

import Data.List (foldl', transpose)
import Data.List.Split (chunksOf)
import Data.Set qualified as Set

type IdTree = (Int, Int)

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
    rows = chunksOf width $ zip [0..] $ concat grid
    cols = transpose rows
    visibleByRow = concatMap visible rows ++ concatMap (visible . reverse) rows
    visibleByCol = concatMap visible cols ++ concatMap (visible . reverse) cols

  print $ length $ Set.fromList $ visibleByRow ++ visibleByCol
